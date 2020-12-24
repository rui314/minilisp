// This software is in the public domain.

#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//======================================================================
// Lisp objects
//======================================================================

// The Lisp object type
enum {
    // Regular objects visible from the user
    TINT = 1,
    TCELL,
    TSYMBOL,
    TPRIMITIVE,
    TFUNCTION,
    TMACRO,
    TSPECIAL,
    TENV,
};

// Subtypes for TSPECIAL
enum {
    TNIL = 1,
    TDOT,
    TCPAREN,
    TTRUE,
};

struct Obj;

// Typedef for the primitive function.
typedef struct Obj *Primitive(struct Obj *env, struct Obj *args);

// The object type
typedef struct Obj {
    // The first word of the object represents the type of the object. Any code that handles object
    // needs to check its type first, then access the following union members.
    int type;

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
            struct Obj *env;
        };
        // Subtype for special type
        int subtype;
        // Environment frame. This is a linked list of association lists
        // containing the mapping from symbols to their value.
        struct {
            struct Obj *vars;
            struct Obj *up;
        };
        // Forwarding pointer
        void *moved;
    };
} Obj;

// Constants
static Obj *Nil;
static Obj *Dot;
static Obj *Cparen;
static Obj *True;

// The list containing all symbols. Such data structure is traditionally called the "obarray", but I
// avoid using it as a variable name as this is not an array but a list.
static Obj *Symbols;

static void error(char *fmt, ...) __attribute((noreturn));

//======================================================================
// Constructors
//======================================================================

static Obj *alloc(int type, size_t size) {
    // Add the size of the type tag.
    size += offsetof(Obj, value);

    // Allocate the object.
    Obj *obj = malloc(size);
    obj->type = type;
    return obj;
}

static Obj *make_int(int value) {
    Obj *r = alloc(TINT, sizeof(int));
    r->value = value;
    return r;
}

static Obj *make_symbol(char *name) {
    Obj *sym = alloc(TSYMBOL, strlen(name) + 1);
    strcpy(sym->name, name);
    return sym;
}

static Obj *make_primitive(Primitive *fn) {
    Obj *r = alloc(TPRIMITIVE, sizeof(Primitive *));
    r->fn = fn;
    return r;
}

static Obj *make_function(int type, Obj *params, Obj *body, Obj *env) {
    assert(type == TFUNCTION || type == TMACRO);
    Obj *r = alloc(type, sizeof(Obj *) * 3);
    r->params = params;
    r->body = body;
    r->env = env;
    return r;
}

static Obj *make_special(int subtype) {
    Obj *r = malloc(sizeof(void *) * 2);
    r->type = TSPECIAL;
    r->subtype = subtype;
    return r;
}

struct Obj *make_env(Obj *vars, Obj *up) {
    Obj *r = alloc(TENV, sizeof(Obj *) * 2);
    r->vars = vars;
    r->up = up;
    return r;
}

static Obj *cons(Obj *car, Obj *cdr) {
    Obj *cell = alloc(TCELL, sizeof(Obj *) * 2);
    cell->car = car;
    cell->cdr = cdr;
    return cell;
}

// Returns ((x . y) . a)
static Obj *acons(Obj *x, Obj *y, Obj *a) {
    return cons(cons(x, y), a);
}

//======================================================================
// Parser
//
// This is a hand-written recursive-descendent parser.
//======================================================================

static Obj *read(void);

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
static Obj *read_list(void) {
    Obj *obj = read();
    if (!obj)
        error("Unclosed parenthesis");
    if (obj == Dot)
        error("Stray dot");
    if (obj == Cparen)
        return Nil;
    Obj *head, *tail;
    head = tail = cons(obj, Nil);

    for (;;) {
        Obj *obj = read();
        if (!obj)
            error("Unclosed parenthesis");
        if (obj == Cparen)
            return head;
        if (obj == Dot) {
            tail->cdr = read();
            if (read() != Cparen)
                error("Closed parenthesis expected after dot");
            return head;
        }
        tail->cdr = cons(obj, Nil);
        tail = tail->cdr;
    }
}

// May create a new symbol. If there's a symbol with the same name, it will not create a new symbol
// but return the existing one.
static Obj *intern(char *name) {
    for (Obj *p = Symbols; p != Nil; p = p->cdr)
        if (strcmp(name, p->car->name) == 0)
            return p->car;
    Obj *sym = make_symbol(name);
    Symbols = cons(sym, Symbols);
    return sym;
}

// Reader marcro ' (single quote). It reads an expression and returns (quote <expr>).
static Obj *read_quote(void) {
    Obj *sym = intern("quote");
    return cons(sym, cons(read(), Nil));
}

static int read_number(int val) {
    while (isdigit(peek()))
        val = val * 10 + (getchar() - '0');
    return val;
}

#define SYMBOL_MAX_LEN 200

static Obj *read_symbol(char c) {
    char buf[SYMBOL_MAX_LEN + 1];
    int len = 1;
    buf[0] = c;
    while (isalnum(peek()) || peek() == '-') {
        if (SYMBOL_MAX_LEN <= len)
            error("Symbol name too long");
        buf[len++] = getchar();
    }
    buf[len] = '\0';
    return intern(buf);
}

static Obj *read(void) {
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
            return read_list();
        if (c == ')')
            return Cparen;
        if (c == '.')
            return Dot;
        if (c == '\'')
            return read_quote();
        if (isdigit(c))
            return make_int(read_number(c - '0'));
        if (c == '-')
            return make_int(-read_number(0));
        if (isalpha(c) || strchr("+=!@#$%^&*", c))
            return read_symbol(c);
        error("Don't know how to handle %c", c);
    }
}

// Prints the given object.
static void print(Obj *obj) {
    switch (obj->type) {
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
    int len = 0;
    for (;;) {
        if (list == Nil)
            return len;
        if (list->type != TCELL)
            error("length: cannot handle dotted list");
        list = list->cdr;
        len++;
    }
}

//======================================================================
// Evaluator
//======================================================================

static Obj *eval(Obj *env, Obj *obj);

static void add_variable(Obj *env, Obj *sym, Obj *val) {
    env->vars = acons(sym, val, env->vars);
}

// Returns a newly created environment frame.
static Obj *push_env(Obj *env, Obj *vars, Obj *values) {
    if (list_length(vars) != list_length(values))
        error("Cannot apply function: number of argument does not match");
    Obj *map = Nil;
    for (Obj *p = vars, *q = values; p != Nil; p = p->cdr, q = q->cdr) {
        Obj *sym = p->car;
        Obj *val = q->car;
        map = acons(sym, val, map);
    }
    return make_env(map, env);
}

// Evaluates the list elements from head and returns the last return value.
static Obj *progn(Obj *env, Obj *list) {
    Obj *r = NULL;
    for (Obj *lp = list; lp != Nil; lp = lp->cdr)
        r = eval(env, lp->car);
    return r;
}

// Evaluates all the list elements and returns their return values as a new list.
static Obj *eval_list(Obj *env, Obj *list) {
    Obj *head = NULL;
    Obj *tail = NULL;
    for (Obj *lp = list; lp != Nil; lp = lp->cdr) {
        Obj *tmp = eval(env, lp->car);
        if (head == NULL) {
            head = tail = cons(tmp, Nil);
        } else {
            tail->cdr = cons(tmp, Nil);
            tail = tail->cdr;
        }
    }
    if (head == NULL)
        return Nil;
    return head;
}

static bool is_list(Obj *obj) {
  return obj == Nil || obj->type == TCELL;
}

// Apply fn with args.
static Obj *apply(Obj *env, Obj *fn, Obj *args) {
    if (!is_list(args))
        error("argument must be a list");
    if (fn->type == TPRIMITIVE)
        return fn->fn(env, args);
    if (fn->type == TFUNCTION) {
        Obj *body = fn->body;
        Obj *params = fn->params;
        Obj *eargs = eval_list(env, args);
        Obj *newenv = push_env(fn->env, params, eargs);
        return progn(newenv, body);
    }
    error("not supported");
}

// Searches for a variable by symbol. Returns null if not found.
static Obj *find(Obj *env, Obj *sym) {
    for (Obj *p = env; p; p = p->up) {
        for (Obj *cell = p->vars; cell != Nil; cell = cell->cdr) {
            Obj *bind = cell->car;
            if (sym == bind->car)
                return bind;
        }
    }
    return NULL;
}

// Expands the given macro application form.
static Obj *macroexpand(Obj *env, Obj *obj) {
    if (obj->type != TCELL || obj->car->type != TSYMBOL)
        return obj;
    Obj *bind = find(env, obj->car);
    if (!bind || bind->cdr->type != TMACRO)
        return obj;
    Obj *args = obj->cdr;
    Obj *body = bind->cdr->body;
    Obj *params = bind->cdr->params;
    Obj *newenv = push_env(env, params, args);
    return progn(newenv, body);
}

// Evaluates the S expression.
static Obj *eval(Obj *env, Obj *obj) {
    switch (obj->type) {
    case TINT:
    case TPRIMITIVE:
    case TFUNCTION:
    case TSPECIAL:
        // Self-evaluating objects
        return obj;
    case TSYMBOL: {
        // Variable
        Obj *bind = find(env, obj);
        if (!bind)
            error("Undefined symbol: %s", obj->name);
        return bind->cdr;
    }
    case TCELL: {
        // Function application form
        Obj *expanded = macroexpand(env, obj);
        if (expanded != obj)
            return eval(env, expanded);
        Obj *fn = eval(env, obj->car);
        Obj *args = obj->cdr;
        if (fn->type != TPRIMITIVE && fn->type != TFUNCTION)
            error("The head of a list must be a function");
        return apply(env, fn, args);
    }
    default:
        error("Bug: eval: Unknown tag type: %d", obj->type);
    }
}

//======================================================================
// Functions and special forms
//======================================================================

// 'expr
static Obj *prim_quote(Obj *env, Obj *list) {
    if (list_length(list) != 1)
        error("Malformed quote");
    return list->car;
}

// (list expr ...)
static Obj *prim_list(Obj *env, Obj *list) {
    return eval_list(env, list);
}

// (setq <symbol> expr)
static Obj *prim_setq(Obj *env, Obj *list) {
    if (list_length(list) != 2 || list->car->type != TSYMBOL)
        error("Malformed setq");
    Obj *bind = find(env, list->car);
    if (!bind)
        error("Unbound variable %s", list->car->name);
    Obj *value = eval(env, list->cdr->car);
    bind->cdr = value;
    return value;
}

// (+ <integer> ...)
static Obj *prim_plus(Obj *env, Obj *list) {
    int sum = 0;
    for (Obj *args = eval_list(env, list); args != Nil; args = args->cdr) {
        if (args->car->type != TINT)
            error("+ takes only numbers");
        sum += args->car->value;
    }
    return make_int(sum);
}

static Obj *handle_function(Obj *env, Obj *list, int type) {
    if (list->type != TCELL || !is_list(list->car) || list->cdr->type != TCELL)
        error("Malformed lambda");
    for (Obj *p = list->car; p != Nil; p = p->cdr) {
        if (p->car->type != TSYMBOL)
            error("Parameter must be a symbol");
        if (!is_list(p->cdr))
            error("Parameter list is not a flat list");
    }
    Obj *car = list->car;
    Obj *cdr = list->cdr;
    return make_function(type, car, cdr, env);
}

// (lambda (<symbol> ...) expr ...)
static Obj *prim_lambda(Obj *env, Obj *list) {
    return handle_function(env, list, TFUNCTION);
}

static Obj *handle_defun(Obj *env, Obj *list, int type) {
    if (list->car->type != TSYMBOL || list->cdr->type != TCELL)
        error("Malformed defun");
    Obj *sym = list->car;
    Obj *rest = list->cdr;
    Obj *fn = handle_function(env, rest, type);
    add_variable(env, sym, fn);
    return fn;
}

// (defun <symbol> (<symbol> ...) expr ...)
static Obj *prim_defun(Obj *env, Obj *list) {
    return handle_defun(env, list, TFUNCTION);
}

// (define <symbol> expr)
static Obj *prim_define(Obj *env, Obj *list) {
    if (list_length(list) != 2 || list->car->type != TSYMBOL)
        error("Malformed define");
    Obj *sym = list->car;
    Obj *value = eval(env, list->cdr->car);
    add_variable(env, sym, value);
    return value;
}

// (defmacro <symbol> (<symbol> ...) expr ...)
static Obj *prim_defmacro(Obj *env, Obj *list) {
    return handle_defun(env, list, TMACRO);
}

// (macroexpand expr)
static Obj *prim_macroexpand(Obj *env, Obj *list) {
    if (list_length(list) != 1)
        error("Malformed macroexpand");
    Obj *body = list->car;
    return macroexpand(env, body);
}

// (println expr)
static Obj *prim_println(Obj *env, Obj *list) {
    print(eval(env, list->car));
    printf("\n");
    return Nil;
}

// (if expr expr expr ...)
static Obj *prim_if(Obj *env, Obj *list) {
    if (list_length(list) < 2)
        error("Malformed if");
    Obj *cond = eval(env, list->car);
    if (cond != Nil) {
        Obj *then = list->cdr->car;
        return eval(env, then);
    }
    Obj *els = list->cdr->cdr;
    return els == Nil ? Nil : progn(env, els);
}

// (= <integer> <integer>)
static Obj *prim_num_eq(Obj *env, Obj *list) {
    if (list_length(list) != 2)
        error("Malformed =");
    Obj *values = eval_list(env, list);
    Obj *x = values->car;
    Obj *y = values->cdr->car;
    if (x->type != TINT || y->type != TINT)
        error("= only takes numbers");
    return x->value == y->value ? True : Nil;
}

// (exit)
static Obj *prim_exit(Obj *env, Obj *list) {
    exit(0);
}

static void add_primitive(Obj *env, char *name, Primitive *fn) {
    Obj *sym = intern(name);
    Obj *prim = make_primitive(fn);
    add_variable(env, sym, prim);
}

static void define_constants(Obj *env) {
    Obj *sym = intern("t");
    add_variable(env, sym, True);
}

static void define_primitives(Obj *env) {
    add_primitive(env, "quote", prim_quote);
    add_primitive(env, "list", prim_list);
    add_primitive(env, "setq", prim_setq);
    add_primitive(env, "+", prim_plus);
    add_primitive(env, "define", prim_define);
    add_primitive(env, "defun", prim_defun);
    add_primitive(env, "defmacro", prim_defmacro);
    add_primitive(env, "macroexpand", prim_macroexpand);
    add_primitive(env, "lambda", prim_lambda);
    add_primitive(env, "if", prim_if);
    add_primitive(env, "=", prim_num_eq);
    add_primitive(env, "println", prim_println);
    add_primitive(env, "exit", prim_exit);
}

//======================================================================
// Entry point
//======================================================================

int main(int argc, char **argv) {
    // Constants and primitives
    Nil = make_special(TNIL);
    Dot = make_special(TDOT);
    Cparen = make_special(TCPAREN);
    True = make_special(TTRUE);
    Symbols = Nil;

    Obj *env = make_env(Nil, NULL);

    define_constants(env);
    define_primitives(env);

    // The main loop
    for (;;) {
        Obj *expr = read();
        if (!expr)
            return 0;
        if (expr == Cparen)
            error("Stray close parenthesis");
        if (expr == Dot)
            error("Stray dot");
        print(eval(env, expr));
        printf("\n");
    }
}
