// This software is in the public domain.
// Originally from: https://github.com/rui314/minilisp

#pragma org 0x1480

#include <cmoc.h>
#include <stdarg.h>
#include <coco.h>

byte disk_buffer[68];
#define sbrk(x) (disk_buffer)
#include <disk.h>
#undef sbrk

#include "setjmp.h"
#include "setjmp.c"

#define bool byte
#define inline
#define __attribute(x)
#define noreturn
#define static
#define ptrdiff_t int
#define false FALSE
#define const
#define NULL 0
#define true TRUE
#define fprintf(x, y, ...) printf(y, __VA_ARGS__) 


jmp_buf jmpbuf;

#define error(...) {\
    printf(__VA_ARGS__); \
    longjmp(&jmpbuf, 0); \
  }

//======================================================================
// Lisp objects
//======================================================================

// The Lisp object type
    // Regular objects visible from the user
#define    TINT 1
#define    TCELL 2
#define    TSYMBOL 3
#define    TPRIMITIVE 4
#define    TFUNCTION 5
#define    TMACRO 6
#define    TENV 7
    // The marker that indicates the object has been moved to other location by GC. The new location
    // can be found at the forwarding pointer. Only the functions to do garbage collection set and
    // handle the object of this type. Other functions will never see the object of this type.
#define    TMOVED 8
    // Const objects. They are statically allocated and will never be managed by GC.
#define    TTRUE 9
#define    TNIL 10
#define    TDOT 11
#define    TCPAREN 12

// Typedef for the primitive function
typedef void *Primitive;

// The object type
typedef struct Obj {
    // The first word of the object represents the type of the object. Any code that handles object
    // needs to check its type first, then access the following union members.
    int type;

    // The total size of the object, including "type" field, this field, the contents, and the
    // padding at the end of the object.
    int size;

    // Object values.
    union val {
        // Int
        int value;
        // Cell
        struct cell {
            struct Obj *car;
            struct Obj *cdr;
        } cell;
        // Symbol
        char name[1];
        // Primitive
        Primitive *fn;
        // Function or Macro
        struct func {
            struct Obj *params;
            struct Obj *body;
            struct Obj *env;
        } func;
        // Environment frame. This is a linked list of association lists
        // containing the mapping from symbols to their value.
        struct env {
            struct Obj *vars;
            struct Obj *up;
        } env;
        // Forwarding pointer
        void *moved;
    } val;
} Obj;

// Constants
static Obj *True;
static Obj *Nil;
static Obj *Dot;
static Obj *Cparen;

// The list containing all symbols. Such data structure is traditionally called the "obarray", but I
// avoid using it as a variable name as this is not an array but a list.
static Obj *Symbols;

//======================================================================
// Memory management
//======================================================================

// The size of the heap in byte
#define MEMORY_SIZE 0x1080

byte memory1[MEMORY_SIZE], memory2[MEMORY_SIZE];

// The pointer pointing to the beginning of the current heap
static void *memory;

// The pointer pointing to the beginning of the old heap
static void *from_space;

// The number of bytes allocated from the heap
static size_t mem_nused = 0;

// Flags to debug GC
static bool gc_running = false;
static bool debug_gc = false;
static bool always_gc = false;

static void gc(void *root);

// Currently we are using Cheney's copying GC algorithm, with which the available memory is split
// into two halves and all objects are moved from one half to another every time GC is invoked. That
// means the address of the object keeps changing. If you take the address of an object and keep it
// in a C variable, dereferencing it could cause SEGV because the address becomes invalid after GC
// runs.
//
// In order to deal with that, all access from C to Lisp objects will go through two levels of
// pointer dereferences. The C local variable is pointing to a pointer on the C stack, and the
// pointer is pointing to the Lisp object. GC is aware of the pointers in the stack and updates
// their contents with the objects' new addresses when GC happens.
//
// The following is a macro to reserve the area in the C stack for the pointers. The contents of
// this area are considered to be GC root.
//
// Be careful not to bypass the two levels of pointer indirections. If you create a direct pointer
// to an object, it'll cause a subtle bug. Such code would work in most cases but fails with SEGV if
// GC happens during the execution of the code. Any code that allocates memory may invoke GC.

#define ROOT_END ((void *)-1)

#define ADD_ROOT(size)                          \
    void *root_ADD_ROOT_[size + 2];             \
    root_ADD_ROOT_[0] = (void *)root;           \
    for (int i = 1; i <= size; i++)             \
        root_ADD_ROOT_[i] = NULL;               \
    root_ADD_ROOT_[size + 1] = ROOT_END;        \
    root = (void *)root_ADD_ROOT_

#define DEFINE1(var1)                           \
    ADD_ROOT(1);                                \
    Obj **var1 = (Obj **)((unsigned int)root_ADD_ROOT_ + 2)

#define DEFINE2(var1, var2)                     \
    ADD_ROOT(2);                                \
    Obj **var1 = (Obj **)((unsigned int)root_ADD_ROOT_ + 2); \
    Obj **var2 = (Obj **)((unsigned int)root_ADD_ROOT_ + 4)

#define DEFINE3(var1, var2, var3)               \
    ADD_ROOT(3);                                \
    Obj **var1 = (Obj **)((unsigned int)root_ADD_ROOT_ + 2); \
    Obj **var2 = (Obj **)((unsigned int)root_ADD_ROOT_ + 4); \
    Obj **var3 = (Obj **)((unsigned int)root_ADD_ROOT_ + 6)

#define DEFINE4(var1, var2, var3, var4)         \
    ADD_ROOT(4);                                \
    Obj **var1 = (Obj **)((unsigned int)root_ADD_ROOT_ + 2); \
    Obj **var2 = (Obj **)((unsigned int)root_ADD_ROOT_ + 4); \
    Obj **var3 = (Obj **)((unsigned int)root_ADD_ROOT_ + 6); \
    Obj **var4 = (Obj **)((unsigned int)root_ADD_ROOT_ + 8)

// Round up the given value to a multiple of size. Size must be a power of 2. It adds size - 1
// first, then zero-ing the least significant bits to make the result a multiple of size. I know
// these bit operations may look a little bit tricky, but it's efficient and thus frequently used.
static inline size_t roundup(size_t var, size_t size) {
    return (var + size - 1) & ~(size - 1);
}

// Allocates memory block. This may start GC if we don't have enough memory.
static Obj *alloc(void *root, int type, size_t size) {
    // The object must be large enough to contain a pointer for the forwarding pointer. Make it
    // larger if it's smaller than that.
    //size = roundup(size, sizeof(void *));

    // Add the size of the type tag and size fields.
    size += offsetof(Obj, val.value);

    // Round up the object size to the nearest alignment boundary, so that the next object will be
    // allocated at the proper alignment boundary. Currently we align the object at the same
    // boundary as the pointer.
    size = roundup(size, sizeof(void *));

    // If the debug flag is on, allocate a new memory space to force all the existing objects to
    // move to new addresses, to invalidate the old addresses. By doing this the GC behavior becomes
    // more predictable and repeatable. If there's a memory bug that the C variable has a direct
    // reference to a Lisp object, the pointer will become invalid by this GC call. Dereferencing
    // that will immediately cause SEGV.
    if (always_gc && !gc_running)
        gc(root);

    // Otherwise, run GC only when the available memory is not large enough.
    if (!always_gc && MEMORY_SIZE < mem_nused + size)
        gc(root);

    // Terminate the program if we couldn't satisfy the memory request. This can happen if the
    // requested size was too large or the from-space was filled with too many live objects.
    if (MEMORY_SIZE < mem_nused + size)
        error("MEMORY EXHAUSTED %x < %x\n", MEMORY_SIZE, mem_nused + size);

    // Allocate the object.
    Obj *obj = (Obj *)(memory + mem_nused);
    obj->type = type;
    obj->size = size;
    mem_nused += size;
    return obj;
}

//======================================================================
// Garbage collector
//======================================================================

// Cheney's algorithm uses two pointers to keep track of GC status. At first both pointers point to
// the beginning of the to-space. As GC progresses, they are moved towards the end of the
// to-space. The objects before "scan1" are the objects that are fully copied. The objects between
// "scan1" and "scan2" have already been copied, but may contain pointers to the from-space. "scan2"
// points to the beginning of the free space.
static Obj *scan1;
static Obj *scan2;

// Moves one object from the from-space to the to-space. Returns the object's new address. If the
// object has already been moved, does nothing but just returns the new address.
static inline Obj *forward(Obj *obj) {
    // If the object's address is not in the from-space, the object is not managed by GC nor it
    // has already been moved to the to-space.
    ptrdiff_t offset = (uint8_t *)obj - (uint8_t *)from_space;
    if (offset < 0 || MEMORY_SIZE <= offset)
        return obj;

    // The pointer is pointing to the from-space, but the object there was a tombstone. Follow the
    // forwarding pointer to find the new location of the object.
    if (obj->type == TMOVED)
        return (Obj *)obj->val.moved;

    // Otherwise, the object has not been moved yet. Move it.
    Obj *newloc = scan2;
    memcpy(newloc, obj, obj->size);
    scan2 = (Obj *)((uint8_t *)scan2 + obj->size);

    // Put a tombstone at the location where the object used to occupy, so that the following call
    // of forward() can find the object's new location.
    obj->type = TMOVED;
    obj->val.moved = (void *)newloc;
    return newloc;
}

// Copies the root objects.
static void forward_root_objects(void *root) {
    Symbols = forward(Symbols);
    for (void **frame = (void **)root; frame; frame = *(void ***)frame)
        for (int i = 1; frame[i] != ROOT_END; i++)
            if (frame[i])
                frame[i] = (void *)forward((Obj *)frame[i]);
}

// Implements Cheney's copying garbage collection algorithm.
// http://en.wikipedia.org/wiki/Cheney%27s_algorithm
static void gc(void *root) {
    assert(!gc_running);
    gc_running = true;

    // Allocate a new semi-space.
    from_space = memory;
    memory = (void *)((from_space == memory1) ? memory2 : memory1);

    // Initialize the two pointers for GC. Initially they point to the beginning of the to-space.
    scan1 = scan2 = (Obj *)memory;

    // Copy the GC root objects first. This moves the pointer scan2.
    forward_root_objects(root);

    // Copy the objects referenced by the GC root objects located between scan1 and scan2. Once it's
    // finished, all live objects (i.e. objects reachable from the root) will have been copied to
    // the to-space.
    while (scan1 < scan2) {
        switch (scan1->type) {
        case TINT:
        case TSYMBOL:
        case TPRIMITIVE:
            // Any of the above types does not contain a pointer to a GC-managed object.
            break;
        case TCELL:
            scan1->val.cell.car = forward(scan1->val.cell.car);
            scan1->val.cell.cdr = forward(scan1->val.cell.cdr);
            break;
        case TFUNCTION:
        case TMACRO:
            scan1->val.func.params = forward(scan1->val.func.params);
            scan1->val.func.body = forward(scan1->val.func.body);
            scan1->val.func.env = forward(scan1->val.func.env);
            break;
        case TENV:
            scan1->val.env.vars = forward(scan1->val.env.vars);
            scan1->val.env.up = forward(scan1->val.env.up);
            break;
        default:
            error("BUG: COPY: UNKNOWN TYPE%d\n", scan1->type);
        }
        scan1 = (Obj *)((uint8_t *)scan1 + scan1->size);
    }

    // Finish up GC.
    size_t old_nused = mem_nused;
    mem_nused = (size_t)((uint8_t *)scan1 - (uint8_t *)memory);
    if (debug_gc)
        fprintf(stderr, "GC: %x/%x\n", mem_nused, old_nused);
    gc_running = false;
}

//======================================================================
// Constructors
//======================================================================

static Obj *make_int(void *root, int value) {
    Obj *r = alloc(root, TINT, sizeof(int));
    r->val.value = value;
    return r;
}

static Obj *cons(void *root, Obj **car, Obj **cdr) {
    Obj *cell = alloc(root, TCELL, sizeof(Obj *) * 2);
    cell->val.cell.car = *car;
    cell->val.cell.cdr = *cdr;
    return cell;
}

static Obj *make_symbol(void *root, char *name) {
    Obj *sym = alloc(root, TSYMBOL, strlen(name) + 1);
    strcpy(sym->val.name, name);
    return sym;
}

static Obj *make_primitive(void *root, Primitive *fn) {
    Obj *r = alloc(root, TPRIMITIVE, sizeof(Primitive *));
    r->val.fn = fn;
    return r;
}

static Obj *make_function(void *root, Obj **env, int type, Obj **params, Obj **body) {
    assert(type == TFUNCTION || type == TMACRO);
    Obj *r = alloc(root, type, sizeof(Obj *) * 3);
    r->val.func.params = *params;
    r->val.func.body = *body;
    r->val.func.env = *env;
    return r;
}

Obj *make_env(void *root, Obj **vars, Obj **up) {
    Obj *r = alloc(root, TENV, sizeof(Obj *) * 2);
    r->val.env.vars = *vars;
    r->val.env.up = *up;
    return r;
}

// Returns ((x . y) . a)
static Obj *acons(void *root, Obj **x, Obj **y, Obj **a) {
    DEFINE1(cell);
    *cell = cons(root, x, y);
    return cons(root, cell, a);
}

//======================================================================
// Parser
//
// This is a hand-written recursive-descendent parser.
//======================================================================

#define SYMBOL_MAX_LEN 200
const char symbol_chars[] = "~!@#$%^&*-_=+:/?<>";

static Obj *read_expr(void *root);

#define stdin 1
#define stderr 1
#define EOF 0

char screen2ascii(char c) {
  if ((c >= 1) && (c <= 26)) {
    return c + 96;
  }
  if ((c >= 96) && (c <= 127)) {
    return c - 64;
  }
  return c;
}


char ascii2screen(char c) {
  if ((c >= 97) && (c <= 122)) {
    return c - 96;
  }
  if ((c >= 32) && (c <= 63)) {
    return c + 64;
  }
  return c;
}


char buffer[512];
byte has_data = false;
char *start_pos = buffer;
char *end_pos = buffer;
bool doing_load = FALSE;
struct FileDesc fd;
bool has_file_data = FALSE;
char file_data;
char getchar() {
  // If there is any buffered file data, return it now
  if (has_file_data) {
    has_file_data = FALSE;
    return file_data;
  }

  // If we are doing a load, read a char
  if (doing_load) {
    word num_read = read(&fd, &file_data, sizeof(file_data));
    // If we ran out of chars, clean up
    if (num_read == 0) {
      close(&fd);
      doing_load = FALSE;
    } else {
      return file_data;
    }
  }

  // If we have data buffered already, simply return that data.
  if (has_data) {
    char c = *start_pos++;
    if (start_pos >= end_pos) {
      has_data = false;
    }
    return screen2ascii(c);
  }

  start_pos = end_pos = *(char **)0x88;
  has_data = true;
  while(true) {
    char c;
    for(c = waitkey(true); c == 0; c = inkey());

    if ((c == 8) && (end_pos > start_pos)) {
      end_pos--;
      *end_pos = ascii2screen(' ');
      *(char **)0x88 = end_pos;
      continue;
    }

    // Don't go over a full screen minus last line
    if (end_pos - start_pos > 512 - 33) {
      continue;
    }

    // Process chars if we hit an ESC
    if ((c == 3) || (c == '\r')) {
      end_pos = *(char **)0x88;
      if (end_pos >= 0x400 + 512 - 32) {
        start_pos = start_pos - 32;
      }
      printf("\n");
      end_pos = *(char **)0x88;

      if (c == 3) {
        memcpy(buffer, start_pos, end_pos - start_pos);
        end_pos = buffer + (end_pos - start_pos);
        start_pos = buffer;
        c = *start_pos++;
        if (start_pos >= end_pos) {
          has_data = false;
        }
        return screen2ascii(c);
      }
      continue;
    }

    // Process a normal char
    if (c >= 32) {
      if (end_pos >= 0x400 + 512 - 1) {
        start_pos = start_pos - 32;
      }
      printf("%c", c);
      end_pos = *(char **)0x88;
    }
  }
}


void ungetc(char c, byte ignore) {
  if (doing_load) {
    file_data = c;
    has_file_data = TRUE;
    return;
  }

  if (start_pos <= buffer) { 
    return;
  }
  has_data = true;
  --start_pos;
  *start_pos = ascii2screen(c);
  return;
}


bool isdigit(char c) {
  return c >= '0' && c <= '9';
}

bool isalnum(char c) {
  return (c >= '0' && c <= '9') || (c >= 'a' && c <='z') ||
         (c >= 'A' && c <= 'Z');
}

bool isalpha(char c) {
  return (c >= 'a' && c <='z') || (c >= 'A' && c <= 'Z');
}

char peek(void) {
    char c = getchar();
    ungetc(c, stdin);
    return c;
}

// Destructively reverses the given list.
static Obj *reverse(Obj *p) {
    Obj *ret = Nil;
    while (p != Nil) {
        Obj *head = p;
        p = p->val.cell.cdr;
        head->val.cell.cdr = ret;
        ret = head;
    }
    return ret;
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
static Obj *read_list(void *root) {
    DEFINE3(obj, head, last);
    *head = Nil;
    for (;;) {
        *obj = read_expr(root);
        if (!*obj)
            error("UNCLOSED PARENTHESIS\n");
        if (*obj == Cparen)
            return reverse(*head);
        if (*obj == Dot) {
            *last = read_expr(root);
            if (read_expr(root) != Cparen)
                error("CLOSED PARENTHESIS EXPECTED AFTER DOT\n");
            Obj *ret = reverse(*head);
            (*head)->val.cell.cdr = *last;
            return ret;
        }
        *head = cons(root, obj, head);
    }
}

// May create a new symbol. If there's a symbol with the same name, it will not create a new symbol
// but return the existing one.
static Obj *intern(void *root, char *name) {
    for (Obj *p = Symbols; p != Nil; p = p->val.cell.cdr)
        if (strcmp(name, p->val.cell.car->val.name) == 0)
            return p->val.cell.car;
    DEFINE1(sym);
    *sym = make_symbol(root, name);
    Symbols = cons(root, sym, &Symbols);
    return *sym;
}

// Reader marcro ' (single quote). It reads an expression and returns (quote <expr>).
static Obj *read_quote(void *root) {
    DEFINE2(sym, tmp);
    *sym = intern(root, "QUOTE");
    *tmp = read_expr(root);
    *tmp = cons(root, tmp, &Nil);
    *tmp = cons(root, sym, tmp);
    return *tmp;
}

static int read_number(int val) {
    while (isdigit(peek()))
        val = val * 10 + (getchar() - '0');
    return val;
}

static Obj *read_symbol(void *root, char c) {
    char buf[SYMBOL_MAX_LEN + 1];
    buf[0] = c;
    int len = 1;
    while (isalnum(peek()) || strchr(symbol_chars, peek())) {
        if (SYMBOL_MAX_LEN <= len)
            error("SYMBOL NAME TOO LONG\n");
        buf[len++] = getchar();
    }
    buf[len] = '\0';
    return intern(root, buf);
}

static Obj *read_expr(void *root) {
    for (;;) {
        char c = getchar();
        if (c == ' ' || c == '\n' || c == '\r' || c == '\t')
            continue;
        if (c == EOF)
            return NULL;
        if (c == ';') {
            skip_line();
            continue;
        }
        if (c == '(')
            return read_list(root);
        if (c == ')')
            return Cparen;
        if (c == '.')
            return Dot;
        if (c == '\'')
            return read_quote(root);
        if (isdigit(c))
            return make_int(root, read_number(c - '0'));
        if (c == '-' && isdigit(peek()))
            return make_int(root, -read_number(0));
        if (isalpha(c) || strchr(symbol_chars, c))
            return read_symbol(root, c);
        error("DON'T KNOW HOW TO HANDLE %c", c);
    }
}

// Prints the given object.
static void print(Obj *obj) {
    switch (obj->type) {
    case TCELL:
        printf("(");
        for (;;) {
            print(obj->val.cell.car);
            if (obj->val.cell.cdr == Nil)
                break;
            if (obj->val.cell.cdr->type != TCELL) {
                printf(" . ");
                print(obj->val.cell.cdr);
                break;
            }
            printf(" ");
            obj = obj->val.cell.cdr;
        }
        printf(")");
        return;

#define CASE(type, ...)                         \
    case type:                                  \
        printf(__VA_ARGS__);                    \
        return
    CASE(TINT, "%d", obj->val.value);
    CASE(TSYMBOL, "%s", obj->val.name);
    CASE(TPRIMITIVE, "<PRIMITIVE>");
    CASE(TFUNCTION, "<FUNCTION>");
    CASE(TMACRO, "<MACRO>");
    CASE(TMOVED, "<MOVED>");
    CASE(TTRUE, "T");
    CASE(TNIL, "()");
#undef CASE
    default:
        error("BUG: PRINT: UNKNOWN TAG TYPE: %d\n", obj->type);
    }
}

// Returns the length of the given list. -1 if it's not a proper list.
static int length(Obj *list) {
    int len = 0;
    for (; list->type == TCELL; list = list->val.cell.cdr)
        len++;
    return list == Nil ? len : -1;
}

//======================================================================
// Evaluator
//======================================================================

static Obj *eval(void *root, Obj **env, Obj **obj);
static Obj *find(Obj **env, Obj *sym);
static void add_variable(void *root, Obj **env, Obj **sym, Obj **val) {
    // Overwrite existing value
    Obj *bind = find(env, *sym);
    if (bind) {
      bind->val.cell.cdr = *val;
      return;
    }

    DEFINE2(vars, tmp);
    *vars = (*env)->val.env.vars;
    *tmp = acons(root, sym, val, vars);
    (*env)->val.env.vars = *tmp;
}

// Returns a newly created environment frame.
static Obj *push_env(void *root, Obj **env, Obj **vars, Obj **vals) {
    DEFINE3(map, sym, val);
    *map = Nil;
    for (; (*vars)->type == TCELL; *vars = (*vars)->val.cell.cdr, *vals = (*vals)->val.cell.cdr) {
        if ((*vals)->type != TCELL)
            error("CANNOT APPLY FUNCTION: NUMBER OF ARGUMENTS DO NOT MATCH\n");
        *sym = (*vars)->val.cell.car;
        *val = (*vals)->val.cell.car;
        *map = acons(root, sym, val, map);
    }
    if (*vars != Nil)
        *map = acons(root, vars, vals, map);
    return make_env(root, map, env);
}

// Evaluates the list elements from head and returns the last return value.
static Obj *progn(void *root, Obj **env, Obj **list) {
    DEFINE2(lp, r);
    for (*lp = *list; *lp != Nil; *lp = (*lp)->val.cell.cdr) {
        *r = (*lp)->val.cell.car;
        *r = eval(root, env, r);
    }
    return *r;
}

// Evaluates all the list elements and returns their return values as a new list.
static Obj *eval_list(void *root, Obj **env, Obj **list) {
    DEFINE4(head, lp, expr, result);
    *head = Nil;
    for (lp = list; *lp != Nil; *lp = (*lp)->val.cell.cdr) {
        *expr = (*lp)->val.cell.car;
        *result = eval(root, env, expr);
        *head = cons(root, result, head);
    }
    return reverse(*head);
}

static bool is_list(Obj *obj) {
    return obj == Nil || obj->type == TCELL;
}

static Obj *apply_func(void *root, Obj **env, Obj **fn, Obj **args) {
    DEFINE3(params, newenv, body);
    *params = (*fn)->val.func.params;
    *newenv = (*fn)->val.func.env;
    *newenv = push_env(root, newenv, params, args);
    *body = (*fn)->val.func.body;
    return progn(root, newenv, body);
}

// Apply fn with args.
static Obj *apply(void *root, Obj **env, Obj **fn, Obj **args) {
    if (!is_list(*args))
        error("ARGUMENT MUST BE A LIST\n");
    if ((*fn)->type == TPRIMITIVE)
        return (Obj *)(*fn)->val.fn(root, env, args);
    if ((*fn)->type == TFUNCTION) {
        DEFINE1(eargs);
        *eargs = eval_list(root, env, args);
        return apply_func(root, env, fn, eargs);
    }
    error("NOT SUPPORTED\n");
}

// Searches for a variable by symbol. Returns null if not found.
static Obj *find(Obj **env, Obj *sym) {
    for (Obj *p = *env; p != Nil; p = p->val.env.up) {
        for (Obj *cell = p->val.env.vars; cell != Nil; cell = cell->val.cell.cdr) {
            Obj *bind = cell->val.cell.car;
            if (sym == bind->val.cell.car)
                return bind;
        }
    }
    return NULL;
}

// Expands the given macro application form.
static Obj *macroexpand(void *root, Obj **env, Obj **obj) {
    if ((*obj)->type != TCELL || (*obj)->val.cell.car->type != TSYMBOL)
        return *obj;
    DEFINE3(bind, macro, args);
    *bind = find(env, (*obj)->val.cell.car);
    if (!*bind || (*bind)->val.cell.cdr->type != TMACRO)
        return *obj;
    *macro = (*bind)->val.cell.cdr;
    *args = (*obj)->val.cell.cdr;
    return apply_func(root, env, macro, args);
}

// Evaluates the S expression.
static Obj *eval(void *root, Obj **env, Obj **obj) {
    switch ((*obj)->type) {
    case TINT:
    case TPRIMITIVE:
    case TFUNCTION:
    case TTRUE:
    case TNIL:
        // Self-evaluating objects
        return *obj;
    case TSYMBOL: {
        // Variable
        Obj *bind = find(env, *obj);
        if (!bind)
            error("UNDEFINED SYMBOL: %s\n", (*obj)->val.name);
        return bind->val.cell.cdr;
    }
    case TCELL: {
        // Function application form
        DEFINE3(fn, expanded, args);
        *expanded = macroexpand(root, env, obj);
        if (*expanded != *obj)
            return eval(root, env, expanded);
        *fn = (*obj)->val.cell.car;
        *fn = eval(root, env, fn);
        *args = (*obj)->val.cell.cdr;
        if ((*fn)->type != TPRIMITIVE && (*fn)->type != TFUNCTION)
            error("THE HEAD OF A LIST MUST BE A FUNCTION\n");
        return apply(root, env, fn, args);
    }
    default:
        error("BUG: EVAL: KNOWN TAG TYPE: %d\n", (*obj)->type);
    }
}

//======================================================================
// Primitive functions and special forms
//======================================================================

// 'expr
static Obj *prim_quote(void *root, Obj **env, Obj **list) {
    if (length(*list) != 1)
        error("MALFORMED QUOTE\n");
    return (*list)->val.cell.car;
}

// (cons expr expr)
static Obj *prim_cons(void *root, Obj **env, Obj **list) {
    if (length(*list) != 2)
        error("MALFORMED CONS\n");
    Obj *cell = eval_list(root, env, list);
    cell->val.cell.cdr = cell->val.cell.cdr->val.cell.car;
    return cell;
}

// (car <cell>)
static Obj *prim_car(void *root, Obj **env, Obj **list) {
    Obj *args = eval_list(root, env, list);
    if (args->val.cell.car->type != TCELL || args->val.cell.cdr != Nil)
        error("MALFORMED CAR\n");
    return args->val.cell.car->val.cell.car;
}

// (cdr <cell>)
static Obj *prim_cdr(void *root, Obj **env, Obj **list) {
    Obj *args = eval_list(root, env, list);
    if (args->val.cell.car->type != TCELL || args->val.cell.cdr != Nil)
        error("MALFORMED CDR\n");
    return args->val.cell.car->val.cell.cdr;
}

// (setq <symbol> expr)
static Obj *prim_setq(void *root, Obj **env, Obj **list) {
    if (length(*list) != 2 || (*list)->val.cell.car->type != TSYMBOL)
        error("MALFORMED SETQ\n");
    DEFINE2(bind, value);
    *bind = find(env, (*list)->val.cell.car);
    if (!*bind)
        error("UNBOUND VARIABLE %s\n", (*list)->val.cell.car->val.name);
    *value = (*list)->val.cell.cdr->val.cell.car;
    *value = eval(root, env, value);
    (*bind)->val.cell.cdr = *value;
    return *value;
}

// (setcar <cell> expr)
static Obj *prim_setcar(void *root, Obj **env, Obj **list) {
    DEFINE1(args);
    *args = eval_list(root, env, list);
    if (length(*args) != 2 || (*args)->val.cell.car->type != TCELL)
        error("MALFORMED SETCAR\n");
    (*args)->val.cell.car->val.cell.car = (*args)->val.cell.cdr->val.cell.car;
    return (*args)->val.cell.car;
}

// (while cond expr ...)
static Obj *prim_while(void *root, Obj **env, Obj **list) {
    if (length(*list) < 2)
        error("MALFORMED WHILE\n");
    DEFINE2(cond, exprs);
    *cond = (*list)->val.cell.car;
    while (eval(root, env, cond) != Nil) {
        *exprs = (*list)->val.cell.cdr;
        eval_list(root, env, exprs);
    }
    return Nil;
}

// (gensym)
static Obj *prim_gensym(void *root, Obj **env, Obj **list) {
  static int count = 0;
  char buf[10];
  //snprintf(buf, sizeof(buf), "G__%d", count++);
  return make_symbol(root, buf);
}

// (+ <integer> ...)
static Obj *prim_plus(void *root, Obj **env, Obj **list) {
    int sum = 0;
    for (Obj *args = eval_list(root, env, list); args != Nil; args = args->val.cell.cdr) {
        if (args->val.cell.car->type != TINT)
            error("+ TAKES ONLY NUMBERS\n");
        sum += args->val.cell.car->val.value;
    }
    return make_int(root, sum);
}


// (* <integer> ...)
static Obj *prim_mult(void *root, Obj **env, Obj **list) {
    int prod = 1;
    for (Obj *args = eval_list(root, env, list); args != Nil; args = args->val.cell.cdr) {
        if (args->val.cell.car->type != TINT)
            error("* TAKES ONLY NUMBERS\n");
        prod *= args->val.cell.car->val.value;
    }
    return make_int(root, prod);
}


// (- <integer> ...)
static Obj *prim_minus(void *root, Obj **env, Obj **list) {
    Obj *args = eval_list(root, env, list);
    for (Obj *p = args; p != Nil; p = p->val.cell.cdr)
        if (p->val.cell.car->type != TINT)
            error("- TAKES ONLY NUMBERS\n");
    if (args->val.cell.cdr == Nil)
        return make_int(root, -args->val.cell.car->val.value);
    int r = args->val.cell.car->val.value;
    for (Obj *p = args->val.cell.cdr; p != Nil; p = p->val.cell.cdr)
        r -= p->val.cell.car->val.value;
    return make_int(root, r);
}

// (< <integer> <integer>)
static Obj *prim_lt(void *root, Obj **env, Obj **list) {
    Obj *args = eval_list(root, env, list);
    if (length(args) != 2)
        error("MALFORMED <\n");
    Obj *x = args->val.cell.car;
    Obj *y = args->val.cell.cdr->val.cell.car;
    if (x->type != TINT || y->type != TINT)
        error("< TAKES ONLY NUMBERS\n");
    return x->val.value < y->val.value ? True : Nil;
}

static Obj *handle_function(void *root, Obj **env, Obj **list, int type) {
    if ((*list)->type != TCELL || !is_list((*list)->val.cell.car) || (*list)->val.cell.cdr->type != TCELL)
        error("MALFORMED LAMBDA\n");
    Obj *p = (*list)->val.cell.car;
    for (; p->type == TCELL; p = p->val.cell.cdr)
        if (p->val.cell.car->type != TSYMBOL)
            error("PARAMETER MUST BE A SYMBOL\n");
    if (p != Nil && p->type != TSYMBOL)
        error("PARAMETER MUST BE A SYMBOL\n");
    DEFINE2(params, body);
    *params = (*list)->val.cell.car;
    *body = (*list)->val.cell.cdr;
    return make_function(root, env, type, params, body);
}

// (lambda (<symbol> ...) expr ...)
static Obj *prim_lambda(void *root, Obj **env, Obj **list) {
    return handle_function(root, env, list, TFUNCTION);
}

static Obj *handle_defun(void *root, Obj **env, Obj **list, int type) {
    if ((*list)->val.cell.car->type != TSYMBOL || (*list)->val.cell.cdr->type != TCELL)
        error("MALFORMED DEFUN\n");
    DEFINE3(fn, sym, rest);
    *sym = (*list)->val.cell.car;
    *rest = (*list)->val.cell.cdr;
    *fn = handle_function(root, env, rest, type);
    add_variable(root, env, sym, fn);
    return *fn;
}

// (defun <symbol> (<symbol> ...) expr ...)
static Obj *prim_defun(void *root, Obj **env, Obj **list) {
    return handle_defun(root, env, list, TFUNCTION);
}

// (define <symbol> expr)
static Obj *prim_define(void *root, Obj **env, Obj **list) {
    if (length(*list) != 2 || (*list)->val.cell.car->type != TSYMBOL)
        error("MALFORMED DEFINE\n");
    DEFINE2(sym, value);
    *sym = (*list)->val.cell.car;
    *value = (*list)->val.cell.cdr->val.cell.car;
    *value = eval(root, env, value);
    add_variable(root, env, sym, value);
    return *value;
}

// (defmacro <symbol> (<symbol> ...) expr ...)
static Obj *prim_defmacro(void *root, Obj **env, Obj **list) {
    return handle_defun(root, env, list, TMACRO);
}

// (macroexpand expr)
static Obj *prim_macroexpand(void *root, Obj **env, Obj **list) {
    if (length(*list) != 1)
        error("MALFORMED MACROEXPAND\n");
    DEFINE1(body);
    *body = (*list)->val.cell.car;
    return macroexpand(root, env, body);
}

// (println expr)
static Obj *prim_println(void *root, Obj **env, Obj **list) {
    DEFINE1(tmp);
    *tmp = (*list)->val.cell.car;
    print(eval(root, env, tmp));
    printf("\n");
    return Nil;
}

// (if expr expr expr ...)
static Obj *prim_if(void *root, Obj **env, Obj **list) {
    if (length(*list) < 2)
        error("MALFORMED IF\n");
    DEFINE3(cond, then, els);
    *cond = (*list)->val.cell.car;
    *cond = eval(root, env, cond);
    if (*cond != Nil) {
        *then = (*list)->val.cell.cdr->val.cell.car;
        return eval(root, env, then);
    }
    *els = (*list)->val.cell.cdr->val.cell.cdr;
    return *els == Nil ? Nil : progn(root, env, els);
}

// (= <integer> <integer>)
static Obj *prim_num_eq(void *root, Obj **env, Obj **list) {
    if (length(*list) != 2)
        error("MALFORMED =\n");
    Obj *values = eval_list(root, env, list);
    Obj *x = values->val.cell.car;
    Obj *y = values->val.cell.cdr->val.cell.car;
    if (x->type != TINT || y->type != TINT)
        error("= ONLY TAKES NUMBERS\n");
    return x->val.value == y->val.value ? True : Nil;
}

// (< <integer> <integer>)
static Obj *prim_num_lt(void *root, Obj **env, Obj **list) {
    if (length(*list) != 2)
        error("MALFORMED <\n");
    Obj *values = eval_list(root, env, list);
    Obj *x = values->val.cell.car;
    Obj *y = values->val.cell.cdr->val.cell.car;
    if (x->type != TINT || y->type != TINT)
        error("< ONLY TAKES NUMBERS\n");
    return x->val.value < y->val.value ? True : Nil;
}

// (> <integer> <integer>)
static Obj *prim_num_gt(void *root, Obj **env, Obj **list) {
    if (length(*list) != 2)
        error("MALFORMED >\n");
    Obj *values = eval_list(root, env, list);
    Obj *x = values->val.cell.car;
    Obj *y = values->val.cell.cdr->val.cell.car;
    if (x->type != TINT || y->type != TINT)
        error("> ONLY TAKES NUMBERS\n");
    return x->val.value > y->val.value ? True : Nil;
}

// (<= <integer> <integer>)
static Obj *prim_num_lte(void *root, Obj **env, Obj **list) {
    if (length(*list) != 2)
        error("MALFORMED <=\n");
    Obj *values = eval_list(root, env, list);
    Obj *x = values->val.cell.car;
    Obj *y = values->val.cell.cdr->val.cell.car;
    if (x->type != TINT || y->type != TINT)
        error("<= ONLY TAKES NUMBERS\n");
    return x->val.value <= y->val.value ? True : Nil;
}

// (>= <integer> <integer>)
static Obj *prim_num_gte(void *root, Obj **env, Obj **list) {
    if (length(*list) != 2)
        error("MALFORMED >=\n");
    Obj *values = eval_list(root, env, list);
    Obj *x = values->val.cell.car;
    Obj *y = values->val.cell.cdr->val.cell.car;
    if (x->type != TINT || y->type != TINT)
        error(">= ONLY TAKES NUMBERS\n");
    return x->val.value >= y->val.value ? True : Nil;
}

// (eq expr expr)
static Obj *prim_eq(void *root, Obj **env, Obj **list) {
    if (length(*list) != 2)
        error("MALFORMED EQ\n");
    Obj *values = eval_list(root, env, list);
    return values->val.cell.car == values->val.cell.cdr->val.cell.car ? True : Nil;
}

static void add_primitive(void *root, Obj **env, char *name, Primitive *fn) {
    DEFINE2(sym, prim);
    *sym = intern(root, name);
    *prim = make_primitive(root, fn);
    add_variable(root, env, sym, prim);
}

static void define_constants(void *root, Obj **env) {
    DEFINE1(sym);
    *sym = intern(root, "T");
    add_variable(root, env, sym, &True);
}


// (load <symbol>)
static Obj *prim_load(void *root, Obj **env, Obj **list) {
    if (doing_load)
      return Nil;

    Obj *args = eval_list(root, env, list);
    if (args->val.cell.car->type != TSYMBOL)
        error("MALFORMED LOAD\n");

    char filebuf[13];
    strncpy(filebuf, args->val.cell.car->val.name, 8);
    strcat(filebuf, ".LSP");
    if (open(&fd, filebuf)) {
      doing_load = TRUE;
    } else {
      return Nil;
    }

    return True;
}


static void define_primitives(void *root, Obj **env) {
    add_primitive(root, env, "QUOTE", (void **)prim_quote);
    add_primitive(root, env, "CONS", (void **)prim_cons);
    add_primitive(root, env, "CAR", (void **)prim_car);
    add_primitive(root, env, "CDR", (void **)prim_cdr);
    add_primitive(root, env, "SETQ", (void **)prim_setq);
    add_primitive(root, env, "SETCAR", (void **)prim_setcar);
    add_primitive(root, env, "WHILE", (void **)prim_while);
    add_primitive(root, env, "GENSYM", (void **)prim_gensym);
    add_primitive(root, env, "+", (void **)prim_plus);
    add_primitive(root, env, "*", (void **)prim_mult);
    add_primitive(root, env, "-", (void **)prim_minus);
    add_primitive(root, env, "<", (void **)prim_lt);
    add_primitive(root, env, "DEFINE", (void **)prim_define);
    add_primitive(root, env, "DEFUN", (void **)prim_defun);
    add_primitive(root, env, "DEFMACRO", (void **)prim_defmacro);
    add_primitive(root, env, "MACROEXPAND", (void **)prim_macroexpand);
    add_primitive(root, env, "LAMBDA", (void **)prim_lambda);
    add_primitive(root, env, "IF", (void **)prim_if);
    add_primitive(root, env, "=", (void **)prim_num_eq);
    add_primitive(root, env, "<", (void **)prim_num_lt);
    add_primitive(root, env, ">", (void **)prim_num_gt);
    add_primitive(root, env, "<=", (void **)prim_num_lte);
    add_primitive(root, env, ">=", (void **)prim_num_gte);
    add_primitive(root, env, "EQ", (void **)prim_eq);
    add_primitive(root, env, "PRINTLN", (void **)prim_println);
    add_primitive(root, env, "LOAD", (void **)prim_load);
}

//======================================================================
// Entry point
//======================================================================

// Returns true if the environment variable is defined and not the empty string.
static bool getEnvFlag(char *name) {
    return false;
}


int main() {
    width(32);
    printf("COLOR COMPUTER MINILISP\n");
    printf("ORIGINAL BY RUI UEYAMA\n");
    printf("COCO PORT: JAMIE CHO\n\n");
    printf("PRESS <BREAK> TO EVAL COMMANDS\n\n");

    // Memory allocation
    memory = (void *)memory1;

    // Init constants
    Obj trueObj, nilObj, dotObj, cparenObj;
    True = &trueObj;
    Nil = &nilObj;
    Dot = &dotObj;
    Cparen = &cparenObj;
    True->type = TTRUE;
    Nil->type = TNIL;
    Dot->type = TDOT;
    Cparen->type = TCPAREN;

    // Constants and primitives
    Symbols = Nil;
    void *root = NULL;
    DEFINE2(env, expr);
    *env = make_env(root, &Nil, &Nil);
    define_constants(root, env);
    define_primitives(root, env);

    // The main loop
    setjmp(&jmpbuf);
    has_file_data = FALSE;
    if (doing_load) {
      doing_load = false;
      close(&fd);
    }
    for (;;) {
        *expr = read_expr(root);
        if (!*expr)
            return 0;
        if (*expr == Cparen)
            error("STRAY CLOSE PARENTHESIS\n");
        if (*expr == Dot)
            error("STRAY DOT\n");
        print(eval(root, env, expr));
        printf("\n");
    }
}
