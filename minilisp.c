// This software is in the public domain.

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
    // Regular objects visible from the user
    TINT = 1,
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

// Subtypes for TSPECIAL
enum {
    TNIL = 1,
    TDOT,
    TCPAREN,
    TTRUE,
};

struct Obj;

// The environment frame to manage variables
typedef struct Env {
    struct Obj *vars;
    struct Env *next;
} Env;

typedef struct Obj *Primitive(Env *env, struct Obj **root, struct Obj **args);

// The object type
typedef struct Obj {
    // The first word of the object represents the type of the object. Any code
    // that handles object needs to check its type first, then access the
    // following union members.
    int type;

    // The total size of the object, including "type" field, this field, the
    // contents, and the padding at the end of the object.
    int size;

    // Object values.
    union {
        // Int
        int value;
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

// Constants
static Obj *Nil;
static Obj *Dot;
static Obj *Cparen;
static Obj *True;

// The list containing all symbols. Such data structure is traditionally called
// the "obarray", but I avoid using it as a variable name as this is not an
// array but a list.
static Obj *Symbols;

// The size of the heap in byte
#define MEMORY_SIZE 4096

// The start of the heap will be aligned to this constant
#define ALIGN (sizeof(void *))

static void *memory;
static size_t mem_nused = 0;
static bool gc_running = false;

// Flags to debug GC
static bool debug_gc = false;
static bool always_gc = false;

static void error(char *fmt, ...) __attribute((noreturn));
static void gc(Env *env, Obj **root);
static void print(Obj *obj);

//======================================================================
// Memory management
//======================================================================

// Currently we are using Cheney's copying GC algorithm, with which the
// available memory is split into two halves and all objects are moved from one
// half to another every time GC is invoked. That means the address of the
// object keeps changing. If you take the address of an object and keep it in a
// C variable, dereferencing it could cause SEGV because the address becomes
// invalid after GC runs.
//
// In order to deal with that, all access from C to Lisp objects will go through
// two levels of pointer dereferences. The C local variable is pointing to a
// pointer on the C stack, and the pointer is pointing to the Lisp object. GC is
// aware of the pointers in the stack and updates their contents with the
// objects' new addresses when GC happens.
//
// The following is a macro to reserve the area in the C stack for the pointers.
// The contents of this area are considered to be GC root.
//
// Be careful not to bypass the two levels of pointer indirections. If you
// create a direct pointer to an object, it'll cause a subtle bug. Such code
// would work in most cases but fails with SEGV if GC happens during the
// execution of the code. Any code that allocates memory may invoke GC.

#define ROOT_END ((Obj *)-1)

#define ADD_ROOT(size)                                          \
    Obj *root_ADD_ROOT_[size+3];                                \
    root_ADD_ROOT_[0] = (Obj *)root;                            \
    root_ADD_ROOT_[1] = (Obj *)__func__;                        \
    root_ADD_ROOT_[size+2] = ROOT_END;                          \
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
    // Add the size of the type tag and size fields.
    size += offsetof(Obj, value);

    // Add a padding at the end of the object, so that the next object will be
    // allocated at the proper alignment boundary.
    if (size % ALIGN != 0)
        size += ALIGN - (size % ALIGN);

    // If the debug flag is on, allocate a new memory space to force all the
    // existing objects to move to new addresses, to invalidate the old
    // addresses. By doing this the GC behavior becomes more predictable and
    // repeatable. If there's a memory bug that the C variable has a direct
    // reference to a Lisp object, the pointer will become invalid by this GC
    // call. Dereferencing that will immediately causes SEGV.
    if (always_gc && !gc_running)
        gc(env, root);

    // Otherwise, run GC only only when the available memory is not large enough.
    if (!always_gc && MEMORY_SIZE < mem_nused + size)
        gc(env, root);

    // Terminate the program if we couldn't satisfy the memory request. This can
    // happen if the requested size was too large or the from-space was filled
    // with too many live objects.
    if (MEMORY_SIZE < mem_nused + size)
        error("Memory exhausted");

    Obj *obj = memory + mem_nused;
    mem_nused += size;
    obj->type = type;
    obj->size = size;
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
    Obj *r = malloc(sizeof(void *) * 2);
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

// Cheney's algorithm uses two pointers to keep track of GC status. At first
// both pointers point to the beginning of the to-space. As GC progresses, they
// are moved towards the end of the to-space. The objects before "scan1" are the
// objects that are fully copied. The objects between "scan1" and "scan2" have
// already been copied, but may contain pointers to the from-space. "scan2"
// points to the beginning of the free space.
Obj *scan1;
Obj *scan2;

// Copies one object from the from-space to the to-space. Returns the object's
// new address. If the object has already been moved, does nothing but just
// returns the new address.
static Obj *forward(Obj *obj) {
    // The object of type TSPECIAL is not managed by GC and will never be
    // copied.
    if (obj->type == TSPECIAL)
        return obj;

    // If the object's address is in the to-space, the object has already been
    // moved.
    ptrdiff_t offset = (void *)obj - memory;
    if (0 <= offset && offset < MEMORY_SIZE)
        return obj;

    // The pointer is pointing to the from-space, but the object there was a
    // tombstone. Follow the forwarding pointer to find the new location of the
    // object.
    if (obj->type == TMOVED)
        return obj->moved;

    // Otherwise, the object has not been moved yet. Move it.
    Obj *newloc = scan2;
    memcpy(newloc, obj, obj->size);
    scan2 = (void *)scan2 + obj->size;

    // Put a tombstone at the location where the object used to occupy, so that
    // the following call of forward() can find the object's new location.
    obj->type = TMOVED;
    obj->moved = newloc;
    return newloc;
}

void *alloc_semispace() {
    return mmap(NULL, MEMORY_SIZE, PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
}

// Copies the root objects.
static void forward_root_objects(Env *env, Obj **root) {
    for (Env *frame = env; frame; frame = frame->next)
        frame->vars = forward(frame->vars);
    Symbols = forward(Symbols);

    for (Obj **rp = root; rp; rp = *(Obj ***)rp)
        for (Obj **ptr = rp + 2; *ptr != ROOT_END; ptr++)
            if (*ptr)
                *ptr = forward(*ptr);
}

// Implements Cheney's copying garbage collection algorithm.
// http://en.wikipedia.org/wiki/Cheney%27s_algorithm
static void gc(Env *env, Obj **root) {
    if (gc_running)
        error("Bug: GC is already running");
    gc_running = true;
    if (debug_gc)
        fprintf(stderr, "Running GC (%lu words used)... ", mem_nused);

    // Allocate the new semi-space.
    void *from_space = memory;
    memory = alloc_semispace();
    if (debug_gc)
        printf("\nMemory: %p + %x\n", memory, MEMORY_SIZE);

    // Initialize the two poitners for GC. Initially they point to the beginning
    // of the to-space.
    scan1 = scan2 = memory;

    // Copy the GC root objects first. This moves the pointer scan2.
    forward_root_objects(env, root);

    // Copy the objects referenced by the GC root objects located between scan1
    // and scan2. Once it's finished, all live objects (i.e. objects reachable
    // from the root) will have been copied to the to-space.
    while (scan1 < scan2) {
        switch (scan1->type) {
        case TINT:
        case TSYMBOL:
        case TPRIMITIVE:
            // Any of the above types does not contain a pointer to a
            // GC-managed object.
            break;
        case TCELL:
            scan1->car = forward(scan1->car);
            scan1->cdr = forward(scan1->cdr);
            break;
        case TFUNCTION:
        case TMACRO:
            scan1->params = forward(scan1->params);
            scan1->body = forward(scan1->body);
            break;
        default:
            error("Bug: copy: unknown type %d", scan1->type);
        }
        scan1 = (void *)scan1 + scan1->size;
    }

    // Finish up GC.
    mem_nused = (void *)scan1 - memory;
    munmap(from_space, MEMORY_SIZE);
    if (debug_gc) {
        fprintf(stderr, "done\n");
        fprintf(stderr, "%lu bytes copied.\n", mem_nused);
    }
    gc_running = false;
}

//======================================================================
// Parser
//
// This is a hand-written recursive-descendent parser.
//======================================================================

static Obj *read(Env *env, Obj **root);

static void error(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(1);
}

static int peek(void) {
    int c = getchar();
    ungetc(c, stdin);
    return c;
}

// Skips the input until newline is found. Newline is one of \r, \r\n or \n.
static void skip_line(void) {
    for (;;) {
        int c = getchar();
        if (c == EOF || c == '\n')
            return;
        if (c == '\r') {
            if (peek() == '\n')
                getchar();
            return;
        }
    }
}

// Reads a list. Note that '(' has already been read.
static Obj *read_list(Env *env, Obj **root) {
    DEFINE4(obj, head, tail, tmp);
    *obj = read(env, root);
    if (!*obj)
        error("Unclosed parenthesis");
    if (*obj == Dot)
        error("Stray dot");
    if (*obj == Cparen)
        return Nil;
    *head = *tail = make_cell(env, root, obj, &Nil);

    for (;;) {
        *obj = read(env, root);
        if (!*obj)
            error("Unclosed parenthesis");
        if (*obj == Cparen)
            return *head;
        if (*obj == Dot) {
            *tmp = read(env, root);
            (*tail)->cdr = *tmp;
            *obj = read(env, root);
            if (*obj != Cparen)
                error("Closed parenthesis expected after dot");
            return *head;
        }
        *tmp = make_cell(env, root, obj, &Nil);
        (*tail)->cdr = *tmp;
        *tail = (*tail)->cdr;
    }
}

// May create a new symbol. If there's a symbol with the same name, it will not
// create a new symbol but return the existing one.
static Obj *intern(Env *env, Obj **root, char *name) {
    for (Obj *p = Symbols; p != Nil; p = p->cdr)
        if (strcmp(name, p->car->name) == 0)
            return p->car;
    DEFINE1(sym);
    *sym = make_symbol(env, root, name);
    Symbols = make_cell(env, root, sym, &Symbols);
    return *sym;
}

static Obj *read_quote(Env *env, Obj **root) {
    DEFINE2(sym, tmp);
    *sym = intern(env, root, "quote");
    *tmp = read(env, root);
    *tmp = make_cell(env, root, tmp, &Nil);
    *tmp = make_cell(env, root, sym, tmp);
    return *tmp;
}

static int read_number(int val) {
    while (isdigit(peek()))
        val = val * 10 + (getchar() - '0');
    return val;
}

#define SYMBOL_MAX_LEN 200

static Obj *read_symbol(Env *env, Obj **root, char c) {
    char buf[SYMBOL_MAX_LEN + 1];
    int len = 1;
    buf[0] = c;
    while (isalnum(peek()) || peek() == '-') {
        if (SYMBOL_MAX_LEN <= len)
            error("Symbol name too long");
        buf[len++] = getchar();
    }
    buf[len] = '\0';
    return intern(env, root, buf);
}

static Obj *read(Env *env, Obj **root) {
    for (;;) {
        int c = getchar();
        if (c == ' ' || c == '\n' || c == '\r' || c == '\t')
            continue;
        if (c == EOF)
            return NULL;
        if (c == ';') {
            skip_line();
            continue;
        }
        if (c == '(')
            return read_list(env, root);
        if (c == ')')
            return Cparen;
        if (c == '.')
            return Dot;
        if (c == '\'')
            return read_quote(env, root);
        if (isdigit(c))
            return make_int(env, root, read_number(c - '0'));
        if (c == '-')
            return make_int(env, root, -read_number(0));
        if (isalpha(c) || strchr("+=!@#$%^&*", c))
            return read_symbol(env, root, c);
        error("Don't know how to handle %c", c);
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
    case TCELL:
        printf("(");
        for (;;) {
            print(obj->car);
            if (obj->cdr == Nil)
                break;
            if (obj->cdr->type != TCELL) {
                printf(" . ");
                print(obj->cdr);
                break;
            }
            printf(" ");
            obj = obj->cdr;
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

static void add_variable(Env *env, Obj **root, Obj **sym, Obj **val) {
    env->vars = acons(env, root, sym, val, &env->vars);
}

static void add_env(Env *env, Obj **root, Env *newenv, Obj **vars, Obj **values) {
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
    if (*head == NULL)
        return Nil;
    return *head;
}

static bool is_list(Obj *obj) {
  return obj == Nil || obj->type == TCELL;
}

static Obj *apply(Env *env, Obj **root, Obj **fn, Obj **args) {
    if ((*fn)->type == TPRIMITIVE) {
        if (!is_list(*args))
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
            Obj *bind = cell->car;
            char *varname = bind->car->name;
            if (strcmp(name, varname) == 0)
                return bind;
        }
    }
    return NULL;
}

// Expands the given macro application form.
static Obj *macroexpand(Env *env, Obj **root, Obj **obj) {
    if ((*obj)->type != TCELL || (*obj)->car->type != TSYMBOL)
        return *obj;
    DEFINE5(bind, macro, args, body, params);
    *bind = find((*obj)->car->name, env);
    if (!*bind || (*bind)->cdr->type != TMACRO)
        return *obj;
    *macro = (*bind)->cdr;
    *args = (*obj)->cdr;
    *body = (*macro)->body;
    *params = (*macro)->params;
    Env newenv;
    add_env(env, root, &newenv, params, args);
    return progn(&newenv, root, body);
}

// Evaluates the S expression.
static Obj *eval(Env *env, Obj **root, Obj **obj) {
    if ((*obj)->type == TINT || (*obj)->type == TPRIMITIVE ||
        (*obj)->type == TFUNCTION || (*obj)->type == TSPECIAL)
        return *obj;
    if ((*obj)->type == TCELL) {
        DEFINE3(fn, expanded, args);
        *expanded = macroexpand(env, root, obj);
        if (*expanded != *obj)
            return eval(env, root, expanded);
        *fn = (*obj)->car;
        *fn = eval(env, root, fn);
        *args = (*obj)->cdr;
        if ((*fn)->type != TPRIMITIVE && (*fn)->type != TFUNCTION)
            error("The head of a list must be a function");
        return apply(env, root, fn, args);
    }
    if ((*obj)->type == TSYMBOL) {
        Obj *bind = find((*obj)->name, env);
        if (!bind)
            error("Undefined symbol: %s", (*obj)->name);
        return bind->cdr;
    }
    error("Bug: eval: Unknown tag type: %d", (*obj)->type);
}

//======================================================================
// Functions and special forms
//======================================================================

static Obj *prim_quote(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 1)
        error("Malformed quote");
    return (*list)->car;
}

static Obj *prim_list(Env *env, Obj **root, Obj **list) {
    return eval_list(env, root, list);
}

static Obj *prim_setq(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 2 || (*list)->car->type != TSYMBOL)
        error("Malformed setq");
    DEFINE2(bind, value);
    *bind = find((*list)->car->name, env);
    if (!*bind)
        error("Unbound variable %s", (*list)->car->name);
    *value = (*list)->cdr->car;
    *value = eval(env, root, value);
    (*bind)->cdr = *value;
    return *value;
}

static Obj *prim_plus(Env *env, Obj **root, Obj **list) {
    DEFINE1(args);
    int sum = 0;
    for (*args = eval_list(env, root, list); *args != Nil; *args = (*args)->cdr) {
        if ((*args)->car->type != TINT)
            error("+ takes only numbers");
        sum += (*args)->car->value;
    }
    return make_int(env, root, sum);
}

static Obj *handle_function(Env *env, Obj **root, Obj **list, int type) {
    if ((*list)->type != TCELL || !is_list((*list)->car) || (*list)->cdr->type != TCELL)
        error("Malformed lambda");
    for (Obj *p = (*list)->car; p != Nil; p = p->cdr) {
        if (p->car->type != TSYMBOL)
            error("Argument must be a symbol");
        if (!is_list(p->cdr))
            error("Argument is not a flat list");
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
        error("Malformed defun");
    }
    DEFINE5(fn, var, sym, rest, tmp);
    *sym = (*list)->car;
    *rest = (*list)->cdr;
    *fn = handle_function(env, root, rest, type);
    add_variable(env, root, sym, fn);
    return *fn;
}

static Obj *prim_defun(Env *env, Obj **root, Obj **list) {
    return handle_defun(env, root, list, TFUNCTION);
}

static Obj *prim_define(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 2 || (*list)->car->type != TSYMBOL)
        error("Malformed setq");
    DEFINE2(sym, value);
    *sym = (*list)->car;
    *value = (*list)->cdr->car;
    *value = eval(env, root, value);
    add_variable(env, root, sym, value);
    return *value;
}

static Obj *prim_defmacro(Env *env, Obj **root, Obj **list) {
    return handle_defun(env, root, list, TMACRO);
}

static Obj *prim_macroexpand(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 1)
        error("Malformed macroexpand");
    DEFINE1(body);
    *body = (*list)->car;
    return macroexpand(env, root, body);
}

static Obj *prim_println(Env *env, Obj **root, Obj **list) {
    DEFINE1(tmp);
    *tmp = (*list)->car;
    print(eval(env, root, tmp));
    printf("\n");
    return Nil;
}

static Obj *prim_if(Env *env, Obj **root, Obj **list) {
    int len = list_length(*list);
    if (len < 2)
        error("Malformed if");
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
        error("Malformed =");
    DEFINE1(values);
    *values = eval_list(env, root, list);
    Obj *x = (*values)->car;
    Obj *y = (*values)->cdr->car;
    if (x->type != TINT || y->type != TINT)
        error("= only takes number");
    return x->value == y->value ? True : Nil;
}

static Obj *prim_gc(Env *env, Obj **root, Obj **list) {
    gc(env, root);
    return Nil;
}

static Obj *prim_exit(Env *env, Obj **root, Obj **list) {
    exit(0);
}

static void add_primitive(Env *env, Obj **root, char *name, Primitive *fn) {
    DEFINE2(prim, sym);
    *prim = make_primitive(env, root, fn);
    *sym = intern(env, root, name);
    add_variable(env, root, sym, prim);
}

static void define_constants(Env *env, Obj **root) {
    DEFINE1(sym);
    *sym = intern(env, root, "t");
    add_variable(env, root, sym, &True);
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

    // Constants and primitives
    Nil = make_special(TNIL);
    Dot = make_special(TDOT);
    Cparen = make_special(TCPAREN);
    True = make_special(TTRUE);
    Symbols = Nil;

    Obj **root = NULL;
    Env env = { Nil, NULL };
    DEFINE1(expr);

    define_constants(&env, root);
    define_primitives(&env, root);

    // The main loop
    for (;;) {
        *expr = read(&env, root);
        if (!*expr)
            return 0;
        if (*expr == Cparen)
            error("Stray close parenthesis");
        if (*expr == Dot)
            error("Stray dot");
        print(eval(&env, root, expr));
        printf("\n");
    }
}
