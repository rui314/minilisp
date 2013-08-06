// This software is in the public domain.

#include <alloca.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

//======================================================================
// Lisp objects
//======================================================================

// The first word of the object in memory represents its type.
enum {
    // Regular objects visible from the user.
    TINT = 1,
    TSTRING,
    TCELL,
    TSYMBOL,
    TPRIMITIVE,
    TFUNCTION,
    TMACRO,
    TSPECIAL,
    // The marker that indicates the object has been moved to other location by
    // GC. The new location can be found at the forwarding pointer. Only the
    // functions to do garbage collection set and handle the object of this
    // type. Other functions will never see the object of this type.
    TMOVED,
};

// Subtypes for TSPECIAL.
enum {
    TNIL = 1,
    TDOT,
    TCPAREN,
    TTRUE,
};

struct Obj;

// The environment frame to manage variables.
typedef struct Env {
    struct Obj *vars;
    struct Env *next;
} Env;

typedef struct Obj *Primitive(Env *env, struct Obj **root, struct Obj **args);

// The object type.
typedef struct Obj {
    // The first word of the object represents the type of the object. Any code
    // that handles object needs to check its type first, then access the
    // following union members.
    int type;

    // Object values.
    union {
        // Int
        int value;
        // String
        char strbody[1];
        // Cell
        struct {
            struct Obj *car;
            struct Obj *cdr;
        };
        // Symbol
        char name[1];
        // Primitive
        Primitive *fn;
        // Function or Macro
        struct {
            struct Obj *params;
            struct Obj *body;
        };
        // Subtype for special type
        int subtype;
        // Forwarding pointer
        void *moved;
    };
} Obj;

// Constants.
static Obj *Nil;
static Obj *Dot;
static Obj *Cparen;
static Obj *True;

// The list containing all symbols. Such data structure is traditionally called
// the "obarray", but I avoid using it as a variable name as this is not an
// array but a list.
static Obj *Symbols;

// The size of the heap in byte.
#define MEMORY_SIZE 4096

// The start of the heap will be aligned to this constant.
#define ALIGN 16

static void *memory;
static int mem_nused;
static int gc_running = 0;

// Flags to debug GC.
static bool debug_gc = false;
static bool always_gc = false;

static void error(char *fmt, ...) __attribute((noreturn));
static Obj *make_cell(Env *env, Obj **root, Obj **car, Obj **cdr);
static void gc(Env *env, Obj **root);
static void print(Obj *obj);

//======================================================================
// Memory management
//======================================================================

// Currently we are using Cheney's copying GC algorithm, which splits the
// available memory space into two halves and moves all objects from one half to
// another every time GC is invoked. That means the address of the object keeps
// changing. You cannot take the address of an object and keep it in a C
// variable because the address will soon become invalid if there's a path to
// the code which may invoke GC.
//
// In order to deal with that, all access from C to Lisp objects will go through
// two levels of pointer dereferences. The C local variable is pointing to a
// pointer on the C stack, and the pointer is pointing to the Lisp object. GC is
// aware of the pointers in the stack and updates their contents with the
// objects new addresses when GC happens.
//
// The following is a macro to reserve the area in the C stack for the pointers.
// The contents of this area are considered to be GC root.
//
// Be careful not to bypass the two levels of pointer indirections. If you
// create a direct pointer to an object, it'll cause a subtle bug. Such code
// would work in most cases but fails with SEGV if GC happens during the
// execution of the code.
#define ADD_ROOT(size)                                          \
    Obj * root_ADD_ROOT_[size+3];                               \
    root_ADD_ROOT_[0] = (Obj *)root;                            \
    root_ADD_ROOT_[1] = (Obj *)__func__;                        \
    root_ADD_ROOT_[size+2] = (Obj *)-1;                         \
    memset(root_ADD_ROOT_ + 2, 0, sizeof(Obj *) * size);        \
    root = root_ADD_ROOT_

#define DEFINE1(var1)                           \
    ADD_ROOT(1);                                \
    Obj **var1 = &root[2]

#define DEFINE2(var1, var2)                     \
    ADD_ROOT(2);                                \
    Obj **var1 = &root[2];                      \
    Obj **var2 = &root[3]

#define DEFINE3(var1, var2, var3)               \
    ADD_ROOT(3);                                \
    Obj **var1 = &root[2];                      \
    Obj **var2 = &root[3];                      \
    Obj **var3 = &root[4]

#define DEFINE4(var1, var2, var3, var4)         \
    ADD_ROOT(4);                                \
    Obj **var1 = &root[2];                      \
    Obj **var2 = &root[3];                      \
    Obj **var3 = &root[4];                      \
    Obj **var4 = &root[5]

#define DEFINE5(var1, var2, var3, var4, var5)   \
    ADD_ROOT(5);                                \
    Obj **var1 = &root[2];                      \
    Obj **var2 = &root[3];                      \
    Obj **var3 = &root[4];                      \
    Obj **var4 = &root[5];                      \
    Obj **var5 = &root[6]

// Allocates memory block. This may start GC if we don't have enough memory.
static Obj *alloc(Env *env, Obj **root, int type, size_t size) {
    size += sizeof(void *);
    if (size % ALIGN != 0)
        size += ALIGN - (size % ALIGN);
    if (always_gc) {
        // Allocate a new memory space to force all objects to move. By doing this
        // all the existing objects' addresses will be invalidated, so if there's a
        // memory bug that'll cause SEGV relatively soon. This should help debug GC.
        if (!gc_running)
            gc(env, root);
    } else if (MEMORY_SIZE < mem_nused + size) {
        gc(env, root);
    }
    if (MEMORY_SIZE < mem_nused + size)
        error("memory exhausted");
    Obj *obj = memory + mem_nused;
    mem_nused += size;
    obj->type = type;
    return obj;
}

//======================================================================
// Constructors
//======================================================================

static Obj *make_int(Env *env, Obj **root, int value) {
    Obj *r = alloc(env, root, TINT, sizeof(int));
    r->value = value;
    return r;
}

static Obj *make_string(Env *env, Obj **root, char *body) {
    Obj *str = alloc(env, root, TSTRING, strlen(body) + 1);
    strcpy(str->strbody, body);
    return str;
}

static Obj *make_cell(Env *env, Obj **root, Obj **car, Obj **cdr) {
    Obj *cell = alloc(env, root, TCELL, sizeof(Obj *) * 2);
    cell->car = *car;
    cell->cdr = *cdr;
    return cell;
}

static Obj *make_symbol(Env *env, Obj **root, char *name) {
    Obj *sym = alloc(env, root, TSYMBOL, strlen(name) + 1);
    strcpy(sym->name, name);
    return sym;
}

static Obj *make_primitive(Env *env, Obj **root, Primitive *fn) {
    Obj *r = alloc(env, root, TPRIMITIVE, sizeof(void *));
    r->fn = fn;
    return r;
}

static Obj *make_function(Env *env, Obj **root, int type, Obj **params, Obj **body) {
    if (type != TFUNCTION && type != TMACRO)
        error("Bug: invalid argument for make_function");
    Obj *r = alloc(env, root, type, sizeof(Obj *) * 2);
    r->params = *params;
    r->body = *body;
    return r;
}

static Obj *make_special(int subtype) {
    Obj *r = malloc(sizeof(int) * 2);
    r->type = TSPECIAL;
    r->subtype = subtype;
    return r;
}

// Returns ((x . y) . a)
static Obj *acons(Env *env, Obj **root, Obj **x, Obj **y, Obj **a) {
    DEFINE1(cell);
    *cell = make_cell(env, root, x, y);
    return make_cell(env, root, cell, a);
}

//======================================================================
// Garbage collector
//======================================================================

// Copies one object from from-space to to-space. Returns the object's new
// address. If the object has already been moved, does nothing but just returns
// the new address.
static Obj *copy(Env *env, Obj **root, Obj **obj) {
    Obj *r;

    // If the object is already in the to-space, it's already moved.
    ptrdiff_t loc = (void *)(*obj) - memory;
    if (0 <= loc && loc < MEMORY_SIZE)
        return *obj;

    switch ((*obj)->type) {
    case TMOVED:
        return (*obj)->moved;
    case TINT:
        r = make_int(env, root, (*obj)->value);
        break;
    case TSTRING:
        r = make_string(env, root, (*obj)->strbody);
        break;
    case TCELL: {
        DEFINE2(car, cdr);
        *car = (*obj)->car;
        *cdr = (*obj)->cdr;
        *car = copy(env, root, car);
        *cdr = copy(env, root, cdr);
        r = make_cell(env, root, car, cdr);
        break;
    }
    case TSYMBOL:
        r = make_symbol(env, root, (*obj)->name);
        break;
    case TPRIMITIVE:
        r = make_primitive(env, root, (*obj)->fn);
        break;
    case TFUNCTION:
    case TMACRO: {
        DEFINE2(params, body);
        *params = (*obj)->params;
        *body = (*obj)->body;
        *params = copy(env, root, params);
        *body = copy(env, root, body);
        r = make_function(env, root, (*obj)->type, params, body);
        break;
    }
    case TSPECIAL:
        // The special objects are not managed by GC. They are created at the
        // startup and will never change their addresses.
        return *obj;
    default:
        error("Bug: copy: unknown type %d", (*obj)->type);
    }
    (*obj)->type = TMOVED;
    (*obj)->moved = r;
    return r;
}

// Helper function for debugging.
static void print_cframe(Obj **root) {
    for (Obj **cframe = root; *cframe; cframe = *(Obj ***)cframe) {
        Obj **ptr = cframe + 2;
        printf(" %s: ", (char *)cframe[1]);
        for (; *ptr != (Obj *)-1; ptr++) {
            if (*ptr)
                print(*ptr);
            else
                printf("- ");
            printf(" ");
        }
        printf("\n");
    }
}

void *alloc_semispace() {
  return mmap(NULL, MEMORY_SIZE, PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
}

// Implements Cheney's copying garbage collection algorithm.
// http://en.wikipedia.org/wiki/Cheney%27s_algorithm
static void gc(Env *env, Obj **root) {
    if (gc_running)
        error("Bug: GC is already running");
    gc_running = 1;
    if (debug_gc)
        fprintf(stderr, "Running GC (%d words used)... ", mem_nused);
    void *old_memory = memory;
    memory = alloc_semispace();
    mem_nused = 0;
    if (debug_gc)
        printf("\nMEMORY: %p + %x\n", memory, MEMORY_SIZE);

    for (Env *frame = env; frame; frame = frame->next)
        frame->vars = copy(env, root, &frame->vars);
    Symbols = copy(env, root, &Symbols);

    if (debug_gc)
        print_cframe(root);

    for (Obj **cframe = root; *cframe; cframe = *(Obj ***)cframe) {
        Obj **ptr = cframe + 2;
        for (; *ptr != (Obj *)-1; ptr++) {
            if (!*ptr) {
                if (debug_gc)
                    printf("Skip\n");
                continue;
            }
            if (debug_gc) {
                printf("Copying %p ", *ptr);
                print(*ptr);
            }
            *ptr = copy(env, root, ptr);
            if (debug_gc) {
                printf(" -> %p ", *ptr);
                print(*ptr);
                printf("\n");
            }
        }
    }
    munmap(old_memory, MEMORY_SIZE);
    if (debug_gc) {
        fprintf(stderr, "done\n");
        fprintf(stderr, "%d bytes copied.\n", mem_nused);
    }
    gc_running = 0;
}

//======================================================================
// Parser
//
// This is a hand-written recursive-descendent parser.
//======================================================================

static Obj *read(Env *env, Obj **root, char **p);
static Obj *read_one(Env *env, Obj **root, char **p);

static void error(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(1);
}

// Read a list. Note that '(' has already been read.
static Obj *read_list(Env *env, Obj **root, char **p) {
    DEFINE4(obj, head, tail, tmp);
    *obj = read_one(env, root, p);
    if (!*obj)
        error("unclosed parenthesis");
    if (*obj == Dot)
        error("stray dot");
    if (*obj == Cparen)
        return Nil;
    (*head) = (*tail) = make_cell(env, root, obj, &Nil);

    for (;;) {
        *obj = read_one(env, root, p);
        if (!*obj)
            error("unclosed parenthesis");
        if (*obj == Cparen)
            return *head;
        if (*obj == Dot) {
            *tmp = read_one(env, root, p);
            (*tail)->cdr = *tmp;
            *obj = read_one(env, root, p);
            if (*obj != Cparen)
              error("Closed parenthesis expected after dot");
            return *head;
        }
        *tmp = make_cell(env, root, obj, &Nil);
        (*tail)->cdr = *tmp;
        (*tail) = (*tail)->cdr;
    }
}

// May create a new symbol. If there's a symbol with the same name, it will not
// create a new symbol but returns the existing one.
static Obj *intern(Env *env, Obj **root, char *name) {
    for (Obj *p = Symbols; p != Nil; p = p->cdr)
        if (strcmp(name, p->car->name) == 0)
            return p->car;
    DEFINE1(sym);
    *sym = make_symbol(env, root, name);
    Symbols = make_cell(env, root, sym, &Symbols);
    return *sym;
}

static Obj *read_quote(Env *env, Obj **root, char **p) {
    DEFINE2(sym, tmp);
    *sym = intern(env, root, "quote");
    *tmp = read(env, root, p);
    *tmp = make_cell(env, root, tmp, &Nil);
    *tmp = make_cell(env, root, sym, tmp);
    return *tmp;
}

static Obj *read_number(Env *env, Obj **root, char **p, int val) {
    for (; isdigit(**p); (*p)++)
        val = val * 10 + (**p - '0');
    return make_int(env, root, val);
}

#define SYMBOL_MAX_LEN 200

static Obj *read_symbol(Env *env, Obj **root, char **p, char c) {
    char buf[SYMBOL_MAX_LEN];
    int len = 1;
    buf[0] = c;
    for (; isalnum(**p) || **p == '-'; (*p)++) {
        if (SYMBOL_MAX_LEN + 1 < len)
            error("symbol name too long");
        buf[len++] = **p;
    }
    buf[len] = '\0';
    return intern(env, root, buf);
}

static Obj *read_one(Env *env, Obj **root, char **p) {
    switch (**p) {
    case ' ': case '\n': case '\r': case '\t':
        (*p)++;
        return read_one(env, root, p);
    case ')':
        (*p)++;
        return Cparen;
    case '.':
        (*p)++;
        return Dot;
    default:
        return read(env, root, p);
    }
}

static Obj *read(Env *env, Obj **root, char **p) {
    for (;;) {
        char c = **p;
        (*p)++;
        if (c == '\0')
            return NULL;
        if (c == ' ' || c == '\n' || c == '\r' || c == '\t')
            continue;
        if (c == '(')
            return read_list(env, root, p);
        if (c == ')')
            error("stray close parenthesis");
        if (c == '\'')
            return read_quote(env, root, p);
        if (isdigit(c))
            return read_number(env, root, p, c - '0');
        if (isalpha(c) || strchr("+=!@#$%^&*", c))
            return read_symbol(env, root, p, c);
        error("don't know how to handle %c", c);
    }
}

// Prints the given object.
static void print(Obj *obj) {
    switch (obj->type) {
    case TMOVED:
        printf("<moved>");
        return;
    case TINT:
        printf("%d", obj->value);
        return;
    case TSTRING:
        printf("%s", obj->strbody);
        return;
    case TCELL:
        printf("(");
        for (;;) {
            print(obj->car);
            if (obj->cdr == Nil) {
                break;
            }
            if (obj->cdr->type == TCELL && !debug_gc) {
                printf(" ");
                obj = obj->cdr;
                continue;
            }
            printf(" . ");
            print(obj->cdr);
            break;
        }
        printf(")");
        return;
    case TSYMBOL:
        printf("%s", obj->name);
        return;
    case TPRIMITIVE:
        printf("<primitive>");
        return;
    case TFUNCTION:
        printf("<function>");
        return;
    case TMACRO:
        printf("<macro>");
        return;
    case TSPECIAL:
        if (obj == Nil)
            printf("()");
        else if (obj == True)
            printf("t");
        else
            error("Bug: print: Unknown subtype: %d", obj->subtype);
        return;
    default:
        error("Bug: print: Unknown tag type: %d", obj->type);
    }
}

static int list_length(Obj *list) {
    if (list == Nil) return 0;
    int len = 1;
    for (;;) {
        if (list->cdr == Nil)
            return len;
        if (list->cdr->type != TCELL)
            error("length: cannot handle incomplete list");
        list = list->cdr;
        len++;
    }
}

//======================================================================
// Evaluator
//======================================================================

static Obj *eval(Env *env, Obj **root, Obj **obj);

static void add_var_int(Env *env, Obj **root, Obj **sym, Obj **val) {
    env->vars = acons(env, root, sym, val, &env->vars);
}

static void add_env(Env *env, Obj **root,  Env *newenv, Obj **vars, Obj **values) {
    if (list_length(*vars) != list_length(*values))
        error("cannot apply function: number of argument does not match");
    DEFINE5(p, q, sym, val, map);
    *map = Nil;
    for (p = vars, q = values; *p != Nil; *p = (*p)->cdr, *q = (*q)->cdr) {
        *val = (*q)->car;
        *sym = (*p)->car;
        *map = acons(env, root, sym, val, map);
    }
    newenv->vars = *map;
    newenv->next = env;
}

static Obj *progn(Env *env, Obj **root, Obj **body) {
    DEFINE1(car);
    for (;;) {
        *car = (*body)->car;
        if ((*body)->cdr == Nil)
            return eval(env, root, car);
        eval(env, root, car);
        *body = (*body)->cdr;
    }
}

static Obj *eval_list(Env *env, Obj **root, Obj **list) {
    DEFINE4(head, tail, lp, tmp);
    for (lp = list; *lp != Nil; *lp = (*lp)->cdr) {
        *tmp = (*lp)->car;
        *tmp = eval(env, root, tmp);
        if (*head == NULL) {
            *head = *tail = make_cell(env, root, tmp, &Nil);
        } else {
            *tmp = make_cell(env, root, tmp, &Nil);
            (*tail)->cdr = *tmp;
            *tail = (*tail)->cdr;
        }
    }
    if (head == NULL)
        error("eval_list: empty list?");
    return *head;
}

static Obj *apply(Env *env, Obj **root, Obj **fn, Obj **args) {
    if ((*fn)->type == TPRIMITIVE) {
        if ((*args) != Nil && (*args)->type != TCELL)
            error("argument must be a list");
        return (*fn)->fn(env, root, args);
    }
    if ((*fn)->type == TFUNCTION) {
        DEFINE3(body, params, eargs);
        *body = (*fn)->body;
        *params = (*fn)->params;
        Env newenv;
        *eargs = eval_list(env, root, args);
        add_env(env, root, &newenv, params, eargs);
        return progn(&newenv, root, body);
    }
    error("not supported");
}

static Obj *find(char *name, Env *env) {
    for (; env; env = env->next) {
        Obj *cell;
        for (cell = env->vars; cell != Nil; cell = cell->cdr) {
            Obj *var = cell->car;
            char *varname = var->car->name;
            if (strcmp(name, varname) == 0)
                return var;
        }
    }
    return NULL;
}

// Expands the given macro application form.
static Obj *macroexpand(Env *env, Obj **root, Obj **obj) {
    if ((*obj)->type != TCELL || (*obj)->car->type != TSYMBOL)
        return *obj;
    DEFINE4(macro, args, body, params);
    *macro = find((*obj)->car->name, env);
    if (!*macro)
        return *obj;
    *macro = (*macro)->cdr;
    if ((*macro)->type != TMACRO)
        return *obj;
    *args = (*obj)->cdr;
    *body = (*macro)->body;
    *params = (*macro)->params;
    Env newenv;
    add_env(env, root, &newenv, params, args);
    return progn(&newenv, root, body);
}

// Evaluates the S expression.
static Obj *eval(Env *env, Obj **root, Obj **obj) {
    if ((*obj)->type == TINT || (*obj)->type == TSTRING ||
        (*obj)->type == TPRIMITIVE || (*obj)->type == TFUNCTION ||
        (*obj)->type == TSPECIAL)
        return *obj;
    if ((*obj)->type == TCELL) {
        DEFINE3(fn, car, args);
        *car = (*obj)->car;
        *args = (*obj)->cdr;
        *fn = eval(env, root, car);
        if ((*fn)->type != TPRIMITIVE && (*fn)->type != TFUNCTION)
            error("Car must be a function");
        return apply(env, root, fn, args);
    }
    if ((*obj)->type == TSYMBOL) {
        Obj *val = find((*obj)->name, env);
        if (!val)
            error("undefined symbol: %s", (*obj)->name);
        return val->cdr;
    }
    error("BUG: eval: Unknown tag type: %d", (*obj)->type);
    return NULL;
}

//======================================================================
// Functions and special forms
//======================================================================

static Obj *prim_quote(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 1)
        error("malformed quote");
    return (*list)->car;
}

static Obj *prim_list(Env *env, Obj **root, Obj **list) {
    return eval_list(env, root, list);
}

static Obj *prim_setq(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 2 ||
        (*list)->car->type != TSYMBOL)
        error("malformed setq");
    DEFINE2(bind, value);
    *bind = find((*list)->car->name, env);
    if (!*bind)
        error("unbound variable", (*list)->car->name);
    *value = (*list)->cdr->car;
    (*bind)->cdr = eval(env, root, value);
    return (*bind)->cdr;
}

static Obj *prim_plus(Env *env, Obj **root, Obj **list) {
    DEFINE1(args);
    *args = eval_list(env, root, list);
    int sum = 0;
    for (;;) {
        if ((*args)->car->type != TINT)
            error("+ takes only numbers");
        sum += (*args)->car->value;
        if ((*args)->cdr == Nil)
            break;
        if ((*args)->cdr->type != TCELL)
            error("+ does not take incomplete list");
        *args = (*args)->cdr;
    }
    return make_int(env, root, sum);
}

static Obj *handle_function(Env *env, Obj **root, Obj **list, int type) {
    if ((*list)->type != TCELL || (*list)->car->type != TCELL ||
        (*list)->cdr->type != TCELL) {
        error("malformed lambda");
    }
    for (Obj *p = (*list)->car; p->cdr != Nil; p = p->cdr) {
        if (p->car->type != TSYMBOL)
            error("argument must be a symbol");
        if (p->cdr->type != TCELL)
            error("argument is not a flat list");
    }
    DEFINE2(car, cdr);
    *car = (*list)->car;
    *cdr = (*list)->cdr;
    return make_function(env, root, type, car, cdr);
}

static Obj *prim_lambda(Env *env, Obj **root, Obj **list) {
    return handle_function(env, root, list, TFUNCTION);
}

static Obj *handle_defun(Env *env, Obj **root, Obj **list, int type) {
    if ((*list)->car->type != TSYMBOL || (*list)->cdr->type != TCELL) {
        error("malformed defun");
    }
    DEFINE5(fn, var, sym, rest, tmp);
    *sym = (*list)->car;
    *rest = (*list)->cdr;
    *fn = handle_function(env, root, rest, type);
    add_var_int(env, root, sym, fn);
    return *fn;
}

static Obj *prim_defun(Env *env, Obj **root, Obj **list) {
    return handle_defun(env, root, list, TFUNCTION);
}

static Obj *prim_define(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 2 || (*list)->car->type != TSYMBOL)
        error("malformed setq");
    DEFINE2(sym, value);
    *sym = (*list)->car;
    *value = (*list)->cdr->car;
    *value = eval(env, root, value);
    add_var_int(env, root, sym, value);
    return *value;
}

static Obj *prim_defmacro(Env *env, Obj **root, Obj **list) {
    return handle_defun(env, root, list, TMACRO);
}

static Obj *prim_macroexpand(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 1)
        error("malformed macroexpand");
    DEFINE1(body);
    *body = (*list)->car;
    return macroexpand(env, root, body);
}

static Obj *prim_println(Env *env, Obj **root, Obj **list) {
    DEFINE1(tmp);
    *tmp = (*list)->car;
    *tmp = eval(env, root, tmp);
    print(*tmp);
    printf("\n");
    return Nil;
}

static Obj *prim_if(Env *env, Obj **root, Obj **list) {
    int len = list_length(*list);
    if (len < 2)
        error("malformed if");
    DEFINE3(cond, then, els);
    *cond = (*list)->car;
    *then = (*list)->cdr->car;
    *cond = eval(env, root, cond);
    if (len == 2)
        return *cond != Nil ? eval(env, root, then) : Nil;
    *els = (*list)->cdr->cdr;
    return *cond != Nil
        ? eval(env, root, then)
        : progn(env, root, els);
}

static Obj *prim_num_eq(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 2)
        error("malformed =");
    DEFINE1(values);
    *values = eval_list(env, root, list);
    if ((*values)->car->type != TINT || (*values)->cdr->car->type != TINT)
        error("= only takes number");
    return (*values)->car->value == (*values)->cdr->car->value ? True : Nil;
}

static Obj *prim_gc(Env *env, Obj **root, Obj **list) {
    gc(env, root);
    return Nil;
}

static Obj *prim_exit(Env *env, Obj **root, Obj **list) {
    exit(0);
}

static void add_var(Env *env, Obj **root, char *name, Obj **var) {
    DEFINE1(sym);
    *sym = intern(env, root, name);
    add_var_int(env, root, sym, var);
}

static void add_primitive(Env *env, Obj **root, char *name, Primitive *fn) {
    DEFINE1(prim);
    *prim = make_primitive(env, root, fn);
    add_var(env, root, name, prim);
}

static void define_consts(Env *env, Obj **root) {
    add_var(env, root, "t", &True);
}

static void define_primitives(Env *env, Obj **root) {
    add_primitive(env, root, "quote", prim_quote);
    add_primitive(env, root, "list", prim_list);
    add_primitive(env, root, "setq", prim_setq);
    add_primitive(env, root, "+", prim_plus);
    add_primitive(env, root, "define", prim_define);
    add_primitive(env, root, "defun", prim_defun);
    add_primitive(env, root, "defmacro", prim_defmacro);
    add_primitive(env, root, "macroexpand", prim_macroexpand);
    add_primitive(env, root, "lambda", prim_lambda);
    add_primitive(env, root, "if", prim_if);
    add_primitive(env, root, "=", prim_num_eq);
    add_primitive(env, root, "println", prim_println);
    add_primitive(env, root, "gc", prim_gc);
    add_primitive(env, root, "exit", prim_exit);
}

//======================================================================
// Entry point
//======================================================================

#define BUFSIZE 250

static bool getEnvFlag(char *name) {
    char *val = getenv(name);
    return val && val[0];
}

int main(int argc, char **argv) {
    // Debug flags
    debug_gc = getEnvFlag("MINILISP_DEBUG_GC");
    always_gc = getEnvFlag("MINILISP_ALWAYS_GC");

    // Memory allocation
    memory = alloc_semispace();
    mem_nused = 0;
    if (debug_gc)
        printf("MEMORY: %p + %x\n", memory, MEMORY_SIZE);

    // Constants and primitives
    Nil = make_special(TNIL);
    Dot = make_special(TDOT);
    Cparen = make_special(TCPAREN);
    True = make_special(TTRUE);
    Symbols = Nil;

    Obj **root = NULL;
    Env env = { Nil, NULL };
    DEFINE2(expr, expanded);

    define_consts(&env, root);
    define_primitives(&env, root);

    // The main loop
    char buf[BUFSIZE];
    for (;;) {
        char *p = buf;
        if (!fgets(p, BUFSIZE, stdin))
          return 0;
        for (;;) {
          *expr = read(&env, root, &p);
          if (!*expr)
              break;
          *expanded = macroexpand(&env, root, expr);
          print(eval(&env, root, expanded));
          printf("\n");
        }
    }
}
