#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <alloca.h>
#include <ctype.h>
#include <sys/mman.h>

enum {
    TMOVED = 7,
    TINT,
    TSTRING,
    TCELL,
    TSYMBOL,
    TPRIMITIVE,
    TFUNCTION,
    TMACRO,
    TSPE,

    // Spetypes
    TNIL = 100,
    TDOT,
    TCPAREN,
    TTRUE,
} Type;

struct Obj;

typedef struct Env {
    struct Obj *vars;
    struct Env *next;
} Env;

typedef struct Obj *Primitive(Env *env, struct Obj **root, struct Obj **args);

typedef struct Obj {
    int type;
    union {
        void *moved;
        // Int
        struct {
            int value;
        };
        // String
        struct {
            char strbody[1];
        };
        // Cell
        struct {
            struct Obj *car;
            struct Obj *cdr;
        };
        // Symbol
        struct {
            char name[1];
        };
        // Primitive
        struct {
            Primitive *fn;
        };
        // Function and Macro
        struct {
            struct Obj *params;
            struct Obj *body;
        };
        // Spe
        struct {
            int spetype;
        };
    };
} Obj;

static Obj *Nil;
static Obj *Dot;
static Obj *Cparen;
static Obj *True;

#define MEMORY_SIZE 4096
#define ALIGN 16

static void *memory;
static int mem_nused;
static int gc_running = 0;
#define DEBUG_GC 0

void error(char *fmt, ...) __attribute((noreturn));
Obj *read(Env *env, Obj **root, char **p);
Obj *read_one(Env *env, Obj **root, char **p);
Obj *make_cell(Env *env, Obj **root, Obj **car, Obj **cdr);
void gc(Env *env, Obj **root);
void print(Obj *obj);

#define ADD_ROOT(size)                                          \
    Obj * root_ADD_ROOT_[size+3];                               \
    root_ADD_ROOT_[0] = (Obj *)root;                            \
    root_ADD_ROOT_[1] = (Obj *)__func__;                        \
    root_ADD_ROOT_[size+2] = (Obj *)-1;                         \
    memset(root_ADD_ROOT_ + 2, 0, sizeof(Obj *) * size);        \
    root = root_ADD_ROOT_;                                      \
    int count_ADD_ROOT_ = 2                                     \

#define NEXT_VAR &root[count_ADD_ROOT_++]

Obj *alloc(Env *env, Obj **root, int type, size_t size) {
    size += sizeof(void *);
    if (size % ALIGN != 0)
        size += ALIGN - (size % ALIGN);
    if (MEMORY_SIZE < mem_nused + size)
        gc(env, root);
    if (MEMORY_SIZE < mem_nused + size)
        error("memory exhausted");
    Obj *obj = memory + mem_nused;
    mem_nused += size;
    obj->type = type;
    return obj;
}

Obj *make_int(Env *env, Obj **root, int value) {
    Obj *r = alloc(env, root, TINT, sizeof(int));
    r->value = value;
    return r;
}

Obj *make_string(Env *env, Obj **root, char *body) {
    Obj *str = alloc(env, root, TSTRING, strlen(body) + 1);
    strcpy(str->strbody, body);
    return str;
}

Obj *make_cell(Env *env, Obj **root, Obj **car, Obj **cdr) {
    Obj *cell = alloc(env, root, TCELL, sizeof(Obj *) * 2);
    cell->car = *car;
    cell->cdr = *cdr;
    return cell;
}

Obj *find(char *name, Env *env);

Obj *make_symbol(Env *env, Obj **root, char *name) {
    Obj *sym = alloc(env, root, TSYMBOL, strlen(name) + 1);
    strcpy(sym->name, name);
    return sym;
}

Obj *make_primitive(Env *env, Obj **root, Primitive *fn) {
    Obj *r = alloc(env, root, TPRIMITIVE, sizeof(void *));
    r->fn = fn;
    return r;
}

Obj *make_function(Env *env, Obj **root, int type, Obj **params, Obj **body) {
    if (type != TFUNCTION && type != TMACRO)
        error("Bug: invalid argument for make_function");
    Obj *r = alloc(env, root, type, sizeof(Obj *) * 2);
    r->params = *params;
    r->body = *body;
    return r;
}

Obj *make_spe(int spetype) {
    Obj *r = malloc(sizeof(int) * 2);
    r->type = TSPE;
    r->spetype = spetype;
    return r;
}

Obj *copy(Env *env, Obj **root, Obj **obj) {
    Obj *r;
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
        ADD_ROOT(2);
        Obj **car = NEXT_VAR;
        Obj **cdr = NEXT_VAR;
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
        ADD_ROOT(2);
        Obj **params = NEXT_VAR;
        Obj **body = NEXT_VAR;
        *params = (*obj)->params;
        *body = (*obj)->body;
        *params = copy(env, root, params);
        *body = copy(env, root, body);
        r = make_function(env, root, (*obj)->type, params, body);
        break;
    }
    case TSPE:
        return *obj;
    default:
        error("Bug: copy: unknown type %d", (*obj)->type);
    }
    (*obj)->type = TMOVED;
    (*obj)->moved = r;
    return r;
}

void print_cframe(Obj **root) {
    Obj **cframe = root;
    for (;;) {
        if (!*cframe) break;
        Obj **ptr = cframe + 2;
        printf(" %s: ", (char *)cframe[1]);
        for (; *ptr != (Obj *)-1; ptr++) {
            if (*ptr) {
                print(*ptr);
            } else {
                printf("- ");
            }
            printf(" ");
        }
        printf("\n");
        cframe = *(Obj ***)cframe;
    }
}

void gc(Env *env, Obj **root) {
    if (gc_running)
        error("Bug: GC is already running");
    gc_running = 1;
    if (DEBUG_GC)
        fprintf(stderr, "Running GC (%d words used)... ", mem_nused);
    void *old_memory = memory;
    memory = mmap(NULL, MEMORY_SIZE, PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
    mem_nused = 0;
    if (DEBUG_GC)
        printf("\nMEMORY: %p + %x\n", memory, MEMORY_SIZE);

    Env *frame;
    for (frame = env; frame; frame = frame->next)
        frame->vars = copy(env, root, &frame->vars);

    if (DEBUG_GC) print_cframe(root);

    Obj **cframe = root;
    for (;;) {
        if (!*cframe) break;
        Obj **ptr = cframe + 2;
        for (; *ptr != (Obj *)-1; ptr++) {
            if (*ptr) {
                if (DEBUG_GC) {
                    printf("Copying %p ", *ptr);
                    print(*ptr);
                }
                *ptr = copy(env, root, ptr);
                if (DEBUG_GC) {
                    printf(" -> %p ", *ptr);
                    print(*ptr);
                    printf("\n");
                }
            } else if (DEBUG_GC) {
                printf("Skip\n");
            }
        }
        cframe = *(Obj ***)cframe;
    }
    munmap(old_memory, MEMORY_SIZE);
    if (DEBUG_GC) {
        fprintf(stderr, "done\n");
        fprintf(stderr, "%d bytes copied.\n", mem_nused);
    }
    gc_running = 0;
}

void error(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(1);
}

Obj *read_sexp(Env *env, Obj **root, char **p) {
    ADD_ROOT(4);
    Obj **obj = NEXT_VAR;
    Obj **head = NEXT_VAR;
    Obj **tail = NEXT_VAR;
    Obj **tmp = NEXT_VAR;
    for (;;) {
        *obj = read_one(env, root, p);
        if (!*obj)
            error("unclosed parenthesis");
        if (*obj == Dot) {
            if (*head == NULL)
                error("stray dot");
            *tmp = read_one(env, root, p);
            (*tail)->cdr = *tmp;
            break;
        }
        if (*obj == Cparen) {
            if (*head == NULL)
                return Nil;
            break;
        }
        if (*head == NULL) {
            (*head) = (*tail) = make_cell(env, root, obj, &Nil);
        } else {
            *tmp = make_cell(env, root, obj, &Nil);
            (*tail)->cdr = *tmp;
            (*tail) = (*tail)->cdr;
        }
    }
    return *head;
}

Obj *intern(Env *env, Obj **root, char *name) {
    Obj *old = find(name, env);
    if (old) return old->car;
    return make_symbol(env, root, name);
}

Obj *read_quote(Env *env, Obj **root, char **p) {
    ADD_ROOT(4);
    Obj **sym = NEXT_VAR;
    Obj **tmp = NEXT_VAR;
    *sym = intern(env, root, "quote");
    *tmp = read(env, root, p);
    *tmp = make_cell(env, root, tmp, &Nil);
    *tmp = make_cell(env, root, sym, tmp);
    return *tmp;
}

Obj *read_number(Env *env, Obj **root, char **p, int val) {
    for (;;) {
        char c = **p;
        switch (c) {
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            (*p)++;
            val = val * 10 + (c - '0');
            break;
        default:
            return make_int(env, root, val);
        }
    }
}

#define SYMBOL_MAX_LEN 200

Obj *read_symbol(Env *env, Obj **root, char **p, char c) {
    char buf[SYMBOL_MAX_LEN];
    int len = 1;
    buf[0] = c;
    for (;;) {
        char c = **p;
        if (isalnum(c) || c == '-') {
            if (SYMBOL_MAX_LEN + 1 < len)
                error("symbol name too long");
            (*p)++;
            buf[len++] = c;
            continue;
        }
        buf[len] = '\0';
        return intern(env, root, buf);
    }
}

Obj *read_one(Env *env, Obj **root, char **p) {
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

Obj *read(Env *env, Obj **root, char **p) {
    for (;;) {
        char c = **p;
        (*p)++;
        if (c == '\0')
            return NULL;
        if (c == ' ' || c == '\n' || c == '\r' || c == '\t')
            continue;
        if (c == '(')
            return read_sexp(env, root, p);
        if (c == ')')
            error("unclosed open parenthesis");
        if (c == '\'')
            return read_quote(env, root, p);
        if (isdigit(c))
            return read_number(env, root, p, c - '0');
        if (isalpha(c) || strchr("+=!@#$%^&*", c))
            return read_symbol(env, root, p, c);
        error("don't know how to handle %c", c);
    }
}

void print(Obj *obj) {
    // if (DEBUG_GC) printf("%p=", obj);
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
            if (obj->cdr->type == TCELL && !DEBUG_GC) {
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
    case TSPE:
        if (obj == Nil)
            printf("()");
        else if (obj == True)
            printf("t");
        else
            error("Bug: print: Unknown SPE type: %d", obj->spetype);
        return;
    default:
        error("Bug: print: Unknown tag type: %d", obj->type);
    }
}

int list_length(Obj *list) {
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

Obj *eval(Env *env, Obj **root, Obj **obj);

void add_var_int(Env *env, Obj **root, Obj **sym, Obj **val) {
    ADD_ROOT(2);
    Obj **cell = NEXT_VAR;
    Obj **tmp = NEXT_VAR;
    *cell = make_cell(env, root, sym, val);
    *tmp = env->vars;
    *tmp = make_cell(env, root, cell, tmp);
    env->vars = *tmp;
}

void add_env(Env *env, Obj **root,  Env *newenv, Obj **vars, Obj **values) {
    if (list_length(*vars) != list_length(*values))
        error("cannot apply function: number of argument does not match");
    ADD_ROOT(6);
    Obj **p = NEXT_VAR;
    Obj **q = NEXT_VAR;
    Obj **sym = NEXT_VAR;
    Obj **val = NEXT_VAR;
    Obj **def = NEXT_VAR;
    Obj **map = NEXT_VAR;
    *map = Nil;
    for (p = vars, q = values; *p != Nil; *p = (*p)->cdr, *q = (*q)->cdr) {
        *val = (*q)->car;
        *sym = (*p)->car;
        *def = make_cell(env, root, sym, val);
        *map = make_cell(env, root, def, map);
    }
    newenv->vars = *map;
    newenv->next = env;
}

void free_env(Env *env) {
    free(env);
}

Obj *progn(Env *env, Obj **root, Obj **body) {
    ADD_ROOT(1);
    Obj **car = NEXT_VAR;
    for (;;) {
        *car = (*body)->car;
        if ((*body)->cdr == Nil)
            return eval(env, root, car);
        eval(env, root, car);
        *body = (*body)->cdr;
    }
}

Obj *eval_list(Env *env, Obj **root, Obj **list) {
    ADD_ROOT(4);
    Obj **head = NEXT_VAR;
    Obj **tail = NEXT_VAR;
    Obj **lp = NEXT_VAR;
    Obj **tmp = NEXT_VAR;
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

Obj *apply(Env *env, Obj **root, Obj **fn, Obj **args) {
    if ((*fn)->type == TPRIMITIVE) {
        if ((*args) != Nil && (*args)->type != TCELL)
            error("argument must be a list");
        return (*fn)->fn(env, root, args);
    }
    if ((*fn)->type == TFUNCTION) {
        ADD_ROOT(3);
        Obj **body = NEXT_VAR;
        Obj **params = NEXT_VAR;
        Obj **eargs = NEXT_VAR;
        *body = (*fn)->body;
        *params = (*fn)->params;
        Env newenv;
        *eargs = eval_list(env, root, args);
        add_env(env, root, &newenv, params, eargs);
        return progn(&newenv, root, body);
    }
    error("not supported");
    return NULL;
}

Obj *find(char *name, Env *env) {
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

Obj *macroexpand(Env *env, Obj **root, Obj **obj) {
    if ((*obj)->type != TCELL || (*obj)->car->type != TSYMBOL)
        return *obj;
    ADD_ROOT(4);
    Obj **macro = NEXT_VAR;
    Obj **args = NEXT_VAR;
    Obj **body = NEXT_VAR;
    Obj **params = NEXT_VAR;
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

Obj *eval(Env *env, Obj **root, Obj **obj) {
    if ((*obj)->type == TINT || (*obj)->type == TSTRING ||
        (*obj)->type == TPRIMITIVE || (*obj)->type == TFUNCTION ||
        (*obj)->type == TSPE)
        return *obj;
    if ((*obj)->type == TCELL) {
        ADD_ROOT(3);
        Obj **fn = NEXT_VAR;
        Obj **car = NEXT_VAR;
        Obj **args = NEXT_VAR;
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

#define BUFSIZE 250

Obj *prim_quote(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 1)
        error("malformed quote");
    return (*list)->car;
}

Obj *prim_list(Env *env, Obj **root, Obj **list) {
    return eval_list(env, root, list);
}

Obj *prim_setq(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 2 ||
        (*list)->car->type != TSYMBOL)
        error("malformed setq");
    ADD_ROOT(2);
    Obj **bind = NEXT_VAR;
    Obj **value = NEXT_VAR;
    *bind = find((*list)->car->name, env);
    if (!*bind)
        error("unbound variable", (*list)->car->name);
    *value = (*list)->cdr->car;
    (*bind)->cdr = eval(env, root, value);
    return (*bind)->cdr;
}

Obj *prim_plus(Env *env, Obj **root, Obj **list) {
    ADD_ROOT(1);
    Obj **args = NEXT_VAR;
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

Obj *handle_function(Env *env, Obj **root, Obj **list, int type) {
    if ((*list)->type != TCELL || (*list)->car->type != TCELL ||
        (*list)->cdr->type != TCELL) {
        error("malformed lambda");
    }
    Obj *p = (*list)->car;
    for (;;) {
        if (p->car->type != TSYMBOL)
            error("argument must be a symbol");
        if (p->cdr == Nil)
            break;
        if (p->cdr->type != TCELL)
            error("argument is not a flat list");
        p = p->cdr;
    }
    ADD_ROOT(2);
    Obj **car = NEXT_VAR;
    Obj **cdr = NEXT_VAR;
    car = &(*list)->car;
    cdr = &(*list)->cdr;
    return make_function(env, root, type, car, cdr);
}

Obj *prim_lambda(Env *env, Obj **root, Obj **list) {
    handle_function(env, root, list, TFUNCTION);
}

Obj *handle_defun(Env *env, Obj **root, Obj **list, int type) {
    if ((*list)->car->type != TSYMBOL || (*list)->cdr->type != TCELL) {
        error("malformed defun");
    }
    ADD_ROOT(5);
    Obj **fn = NEXT_VAR;
    Obj **var = NEXT_VAR;
    Obj **sym = NEXT_VAR;
    Obj **rest = NEXT_VAR;
    Obj **tmp = NEXT_VAR;
    *sym = (*list)->car;
    *rest = (*list)->cdr;
    *fn = handle_function(env, root, rest, type);
    add_var_int(env, root, sym, fn);
    return *fn;
}

Obj *prim_defun(Env *env, Obj **root, Obj **list) {
    return handle_defun(env, root, list, TFUNCTION);
}

Obj *prim_define(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 2 ||
        (*list)->car->type != TSYMBOL)
        error("malformed setq");
    ADD_ROOT(2);
    Obj **sym = NEXT_VAR;
    Obj **value = NEXT_VAR;
    *sym = (*list)->car;
    *value = (*list)->cdr->car;
    *value = eval(env, root, value);
    add_var_int(env, root, sym, value);
    return *value;
}

Obj *prim_defmacro(Env *env, Obj **root, Obj **list) {
    return handle_defun(env, root, list, TMACRO);
}

Obj *prim_macroexpand(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 1)
        error("malformed macroexpand");
    ADD_ROOT(1);
    Obj **body = NEXT_VAR;
    *body = (*list)->car;
    return macroexpand(env, root, body);
}

Obj *prim_println(Env *env, Obj **root, Obj **list) {
    ADD_ROOT(1);
    Obj **tmp = NEXT_VAR;
    *tmp = (*list)->car;
    *tmp = eval(env, root, tmp);
    print(*tmp);
    printf("\n");
    return Nil;
}

Obj *prim_if(Env *env, Obj **root, Obj **list) {
    int len = list_length(*list);
    if (len < 2)
        error("malformed if");
    ADD_ROOT(3);
    Obj **cond = NEXT_VAR;
    Obj **then = NEXT_VAR;
    Obj **els = NEXT_VAR;
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

Obj *prim_num_eq(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 2)
        error("malformed =");
    ADD_ROOT(1);
    Obj **values = NEXT_VAR;
    *values = eval_list(env, root, list);
    if ((*values)->car->type != TINT || (*values)->cdr->car->type != TINT)
        error("= only takes number");
    return (*values)->car->value == (*values)->cdr->car->value ? True : Nil;
}

Obj *prim_gc(Env *env, Obj **root, Obj **list) {
    gc(env, root);
    return Nil;
}

Obj *prim_exit(Env *env, Obj **root, Obj **list) {
    exit(0);
}

void add_var(Env *env, Obj **root, char *name, Obj **var) {
    ADD_ROOT(3);
    Obj **sym = NEXT_VAR;
    Obj **cell = NEXT_VAR;
    Obj **tmp = NEXT_VAR;
    *sym = intern(env, root, name);
    add_var_int(env, root, sym, var);
}

void add_primitive(Env *env, Obj **root, char *name, Primitive *fn) {
    ADD_ROOT(4);
    Obj **prim = NEXT_VAR;
    *prim = make_primitive(env, root, fn);
    add_var(env, root, name, prim);
}

void define_consts(Env *env, Obj **root) {
    add_var(env, root, "t", &True);
}

void define_primitives(Env *env, Obj **root) {
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

int main(int argc, char **argv) {
    Obj **root = NULL;
    ADD_ROOT(2);
    Obj **sexp = NEXT_VAR;
    Obj **expanded = NEXT_VAR;

    memory = mmap(NULL, MEMORY_SIZE, PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
    mem_nused = 0;
    if (DEBUG_GC)
        printf("MEMORY: %p + %x\n", memory, MEMORY_SIZE);

    Nil = make_spe(TNIL);
    Dot = make_spe(TDOT);
    Cparen = make_spe(TCPAREN);
    True = make_spe(TTRUE);

    Env *env = malloc(sizeof(Env));
    env->vars = Nil;
    env->next = NULL;

    define_consts(env, root);
    define_primitives(env, root);

    mem_nused = MEMORY_SIZE - sizeof(void *) * 32;

    char buf[BUFSIZE];
    for (;;) {
        char *p = buf;
        fgets(p, BUFSIZE, stdin);
        *sexp = read(env, root, &p);
        if (!*sexp) continue;
        *expanded = macroexpand(env, root, sexp);
        print(eval(env, root, expanded));
        printf("\n");
    }
    return 0;
}
