//// This software is in the public domain.
// Originally from: https://github.com/rui314/minilisp

#define VERSION "Color Computer MiniLisp 0.6\n"

#include <cmoc.h>
#include <stdarg.h>
#include <coco.h>

#define bool byte
#define inline
#define __attribute(x)
#define noreturn
#define static
#define ptrdiff_t int
#define false FALSE
#undef NULL
#define NULL 0
#define true TRUE

#include <disk.h>
#include "setjmp.h"
#include "setjmp.c"


// Macros to help us with print operations
#define bprintf(...) { swap_in_basic_for_print(); printf(__VA_ARGS__); swap_out_basic_after_print(); }
#define fprintf(x, y, ...) bprintf(y, __VA_ARGS__)


// We map in an area of memory from 0x8000 to 0xA000 to store our input buffer
#define MAP_IN_INPUT_BUFFER { \
  asm { lda #$34 }\
  asm { sta $ffa4 }\
}
#define MAP_OUT_INPUT_BUFFER { \
  asm { lda #$30 }\
  asm { sta $ffa4 }\
}


// When we perform print operations, we store the old stack pointer and move the stack
// to 0x600. This is because BASIC maps the 40 and 80 column screen memory to
// 0x2000 and 0x4000 which will overlap with our stack. 0x500 is in the middle of
// the 32x16 text screen memory.
void *stack_ptr;

// Swaps Basic back in, turns on interrupts, moves stack
// so that print opertions will work.
asm void swap_in_basic_for_print() {
  asm {
    orcc #$50
    puls d
    sts stack_ptr
    lds #$600
    pshs d
    bra swap_in_basic
  }
}

// Swaps Basic back out, turns on interrupts and restores
// the stack after a print operation
asm void swap_out_basic_after_print() {
  asm {
    orcc #$50
    puls d
    lds stack_ptr
    pshs d
    bra swap_out_basic
  }
}

// Swaps Basic back in and turns on interrupts.
asm void swap_in_basic() {
  asm {
    ldd #$3c3d
    sta $ffa4
    stb $ffa5
    ldd #$3e3f
    sta $ffa6
    stb $ffa7
    andcc #$af
    rts
  }
}

// Swaps Basic back out and turns off interrupts.
asm void swap_out_basic() {
  asm {
    orcc #$50
    ldd #$3031
    sta $ffa4
    stb $ffa5
    ldd #$3233
    sta $ffa6
    stb $ffa7
    rts
  }
}

bool doing_load = FALSE;
void bclose(struct FileDesc *fd) {
    doing_load = FALSE;
    swap_in_basic();
    setHighSpeed(FALSE);
    close(fd);
    setHighSpeed(TRUE);
    swap_out_basic();
}

word bread(struct FileDesc *fd, char *buf, word numBytesRequested) {
    swap_in_basic();
    setHighSpeed(FALSE);
    word num_read = read(fd, buf, numBytesRequested);
    setHighSpeed(TRUE);
    swap_out_basic();
    return num_read;
}

byte bopen(struct FileDesc *fd, char *filename) {
    swap_in_basic();
    setHighSpeed(FALSE);
    byte bopen_result = openfile(fd, filename);
    setHighSpeed(TRUE);
    swap_out_basic();
    doing_load = bopen_result;
    return bopen_result;
}

jmp_buf jmpbuf;

#define error(...) {\
    bprintf(__VA_ARGS__); \
    longjmp(&jmpbuf, 0); \
  }

#define error2(x, y) {\
    memcpy(generic_buffer, y, sizeof(generic_buffer)-1); \
    generic_buffer[sizeof(generic_buffer)-1] = '\0'; \
    bprintf(x, generic_buffer); \
    longjmp(&jmpbuf, 0); \
  }

//======================================================================
// Lisp objects
//======================================================================

// The Lisp object type
    // Regular objects visible from the user
#define    TLONG 1
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
        // long
        long value;
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
#define MEMORY_SIZE 15360

byte *memory1 = (byte *)0x8000;
byte *memory2 = memory1 + MEMORY_SIZE;

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
        error("Memory exhausted %x < %x\n", MEMORY_SIZE, mem_nused + size);

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
        case TLONG:
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
            error("Bug: copy: unknown type %d\n", scan1->type);
        }
        scan1 = (Obj *)((uint8_t *)scan1 + scan1->size);
    }

    // Finish up GC.
    size_t old_nused = mem_nused;
    mem_nused = (size_t)((uint8_t *)scan1 - (uint8_t *)memory);
    byte sp;
    if (debug_gc)
        fprintf(stderr, "GC: %x/%x  SP: %x\n", mem_nused, old_nused, &sp);
    gc_running = false;
}

//======================================================================
// Constructors
//======================================================================

static Obj *make_long(void *root, long value) {
    Obj *r = alloc(root, TLONG, sizeof(long));
    r->val.value = value;
    return r;
}

static Obj *cons(void *root, Obj **car, Obj **cdr) {
    Obj *cell = alloc(root, TCELL, sizeof(Obj *) * 2);
    cell->val.cell.car = *car;
    cell->val.cell.cdr = *cdr;
    return cell;
}

static Obj *make_symbol(void *root, const char *name) {
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

#define SYMBOL_MAX_LEN 50
byte generic_buffer[SYMBOL_MAX_LEN + 1];
const char symbol_chars[] = "~!@#$%^&*-_=+:/?<>";

static Obj *read_expr(void *root);

#define stdin 1
#define stderr 1
#define EOF 0


#define SCREEN_WIDTH 80
#define INPUT_BUFFER_SIZE (SCREEN_WIDTH * SCREEN_BUFFER_HEIGHT)
#define screen_ptr ((char **)0xfe00)
#define SCREEN_BYTES_PER_CHAR 2
#define screen_x ((char *)0xfe02)
#define screen_y ((char *)0xfe03)
#define SCREEN_BUFFER_HEIGHT 24
char *buffer = (char *)0x8000;
byte has_data = false;
char *start_pos;
char *end_pos;
struct FileDesc fd;
bool has_file_data = FALSE;
char file_data;
char last_char;
char getchar() {
  // If there is any buffered file data, return it now
  if (has_file_data) {
    has_file_data = FALSE;
    return file_data;
  }

  // If we are doing a load, read a char
  if (doing_load) {
    word num_read = bread(&fd, &file_data, sizeof(file_data));
    // If we ran out of chars, clean up
    if (num_read == 0) {
      bclose(&fd);
    } else {
      return file_data;
    }
  }

  // If we have data buffered already, simply return that data.
  if (has_data) {
    MAP_IN_INPUT_BUFFER;
    char c = *start_pos++;
    MAP_OUT_INPUT_BUFFER;
    if (start_pos >= end_pos) {
      has_data = false;
    }
    return c;
  }

  start_pos = end_pos = buffer;
  has_data = true;
  while(true) {
    swap_in_basic_for_print();
    last_char = waitkey(true);
    swap_out_basic_after_print();

    // Process backspace
    char c = last_char;
    if ((c == 8) && (end_pos > start_pos)) {
      bprintf("%c", c);
      end_pos--;
      MAP_IN_INPUT_BUFFER;
      *end_pos = ' ';
      MAP_OUT_INPUT_BUFFER;
      continue;
    }

    // Process chars if we hit enter or ESC
    if ((c == 3) || (c == '\r')) {
      // Don't go over a full screen minus last line
      if (c == '\r') {
        if ((end_pos - start_pos) >= (INPUT_BUFFER_SIZE - SCREEN_BYTES_PER_CHAR * SCREEN_WIDTH)) {
          continue;
        }
      }

      // Increment by one line
      char *before = *screen_ptr;
      bprintf("\n");
      char *after = *screen_ptr;
      if (after <= before) {
        after = after + SCREEN_BYTES_PER_CHAR * SCREEN_WIDTH;
      }

      // Fill in buffer with spaces
      unsigned delta = (after - before) / SCREEN_BYTES_PER_CHAR;
      MAP_IN_INPUT_BUFFER;
      memset(end_pos, ' ', delta);
      *end_pos = '\r';
      MAP_OUT_INPUT_BUFFER;
      end_pos += delta;

      // If break was pressed, begin processing data
      if (c == 3) {
        MAP_IN_INPUT_BUFFER;
        c = *start_pos++;
        MAP_OUT_INPUT_BUFFER;
        if (start_pos > end_pos) {
          has_data = false;
        }
        return c;
      }
      continue;
    }

    // Process a normal char
    if (end_pos - start_pos >= (INPUT_BUFFER_SIZE - 1)) {
      continue;
    }
    if (c >= ' ') {
      if (end_pos >= start_pos + INPUT_BUFFER_SIZE) {
        start_pos = start_pos - SCREEN_WIDTH;
      }
      bprintf("%c", c);
      MAP_IN_INPUT_BUFFER;
      *end_pos++ = c;
      MAP_OUT_INPUT_BUFFER;
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
  MAP_IN_INPUT_BUFFER;
  *start_pos = c;
  MAP_OUT_INPUT_BUFFER;
  return;
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
            error("Unclosed parenthesis\n");
        if (*obj == Cparen)
            return reverse(*head);
        if (*obj == Dot) {
            *last = read_expr(root);
            if (read_expr(root) != Cparen)
                error("Closed parenthesis expected after dot\n");
            Obj *ret = reverse(*head);
            (*head)->val.cell.cdr = *last;
            return ret;
        }
        *head = cons(root, obj, head);
    }
}

// May create a new symbol. If there's a symbol with the same name, it will not create a new symbol
// but return the existing one.
static Obj *intern(void *root, const char *name) {
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
    *sym = intern(root, "quote");
    *tmp = read_expr(root);
    *tmp = cons(root, tmp, &Nil);
    *tmp = cons(root, sym, tmp);
    return *tmp;
}

static long read_number(long val) {
    while (isdigit(peek()))
        val = val * 10 + (getchar() - '0');
    return val;
}

static Obj *read_symbol(void *root, char c) {
    char *buf = (char *)generic_buffer;
    buf[0] = c;
    int len = 1;
    while (isalnum(peek()) || strchr(symbol_chars, peek())) {
        if (SYMBOL_MAX_LEN <= len)
            error("Symbol name too long\n");
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
            return make_long(root, read_number(c - '0'));
        if (c == '-' && isdigit(peek()))
            return make_long(root, -read_number(0));
        if (isalpha(c) || strchr(symbol_chars, c))
            return read_symbol(root, c);
        error("Don't know how to handle %c", c);
    }
}

// Prints the given object.
static void print(Obj *obj) {
    long tmp;
    switch (obj->type) {
    case TCELL:
        bprintf("(");
        for (;;) {
            print(obj->val.cell.car);
            if (obj->val.cell.cdr == Nil)
                break;
            if (obj->val.cell.cdr->type != TCELL) {
                bprintf(" . ");
                print(obj->val.cell.cdr);
                break;
            }
            bprintf(" ");
            obj = obj->val.cell.cdr;
        }
        bprintf(")");
        return;

#define CASE(type, ...)                         \
    case type:                                  \
        bprintf(__VA_ARGS__);                   \
        return
#define CASE2(type, x, y)                                        \
    case type:                                                   \
        memcpy(generic_buffer, y, sizeof(generic_buffer) - 1);   \
        generic_buffer[sizeof(generic_buffer)-1] = '\0';         \
        bprintf(x, generic_buffer);                              \
        return
#define CASE3(type, x, y)                                        \
    case type:                                                   \
        tmp = y;                                                 \
        bprintf(x, tmp);                                         \
        return
    CASE3(TLONG, "%ld", obj->val.value);
    CASE2(TSYMBOL, "%s", obj->val.name);
    CASE(TPRIMITIVE, "<primitive>");
    CASE(TFUNCTION, "<function>");
    CASE(TMACRO, "<macro>");
    CASE(TMOVED, "<moved>");
    CASE(TTRUE, "T");
    CASE(TNIL, "()");
#undef CASE
#undef CASE2
    default:
        error("Bug: print: unknown tag type: %d\n", obj->type);
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
            error("Cannot apply function: number of arguments do not match\n");
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

typedef Obj *(*PrimitiveFuncPtr)(void *, Obj **, Obj **);

// Apply fn with args.
static Obj *apply(void *root, Obj **env, Obj **fn, Obj **args) {
    if (!is_list(*args))
        error("Argument must be a list\n");
    if ((*fn)->type == TPRIMITIVE)
        return (Obj *) ((PrimitiveFuncPtr) (*fn)->val.fn)(root, env, args);
    if ((*fn)->type == TFUNCTION) {
        DEFINE1(eargs);
        *eargs = eval_list(root, env, args);
        return apply_func(root, env, fn, eargs);
    }
    error("Not supported\n");
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
    case TLONG:
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
            error2("Undefined symbol: %s\n", (*obj)->val.name);
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
            error("The head of a list must be a function\n");
        return apply(root, env, fn, args);
    }
    default:
        error("Bug: eval: known tag type: %d\n", (*obj)->type);
    }
}

//======================================================================
// Primitive functions and special forms
//======================================================================

// 'expr
static Obj *prim_quote(void *root, Obj **env, Obj **list) {
    if (length(*list) != 1) {
        error("Malformed %s\n", "quote");
    }
    return (*list)->val.cell.car;
}

// (cons expr expr)
static Obj *prim_cons(void *root, Obj **env, Obj **list) {
    if (length(*list) != 2) {
        error("Malformed %s\n", "cons");
    }
    Obj *cell = eval_list(root, env, list);
    cell->val.cell.cdr = cell->val.cell.cdr->val.cell.car;
    return cell;
}

// (car <cell>)
static Obj *prim_car(void *root, Obj **env, Obj **list) {
    Obj *args = eval_list(root, env, list);
    if (args->val.cell.car->type != TCELL || args->val.cell.cdr != Nil) {
        error("Malformed %s\n", "car");
    }
    return args->val.cell.car->val.cell.car;
}

// (cdr <cell>)
static Obj *prim_cdr(void *root, Obj **env, Obj **list) {
    Obj *args = eval_list(root, env, list);
    if (args->val.cell.car->type != TCELL || args->val.cell.cdr != Nil) {
        int v = args->val.cell.car->type;
        error("Malformed %s\n", "cdr");
    }
    return args->val.cell.car->val.cell.cdr;
}

// (setq <symbol> expr)
static Obj *prim_setq(void *root, Obj **env, Obj **list) {
    if (length(*list) != 2 || (*list)->val.cell.car->type != TSYMBOL) {
        error("Malformed %s\n", "setq");
    }
    DEFINE2(bind, value);
    *bind = find(env, (*list)->val.cell.car);
    if (!*bind)
        error("Unbound variable %s\n", (*list)->val.cell.car->val.name);
    *value = (*list)->val.cell.cdr->val.cell.car;
    *value = eval(root, env, value);
    (*bind)->val.cell.cdr = *value;
    return *value;
}

// (setcar <cell> expr)
static Obj *prim_setcar(void *root, Obj **env, Obj **list) {
    DEFINE1(args);
    *args = eval_list(root, env, list);
    if (length(*args) != 2 || (*args)->val.cell.car->type != TCELL) {
        error("Malformed %s\n", "setcar");
    }
    (*args)->val.cell.car->val.cell.car = (*args)->val.cell.cdr->val.cell.car;
    return (*args)->val.cell.car;
}

// (while cond expr ...)
static Obj *prim_while(void *root, Obj **env, Obj **list) {
    if (length(*list) < 2) {
        error("Malformed %s\n", "while");
    }
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
    long sum = 0;
    for (Obj *args = eval_list(root, env, list); args != Nil; args = args->val.cell.cdr) {
        if (args->val.cell.car->type != TLONG) {
            error("%s takes only numbers\n", "+");
        }
        sum += args->val.cell.car->val.value;
    }
    return make_long(root, sum);
}


// (* <integer> ...)
static Obj *prim_mult(void *root, Obj **env, Obj **list) {
    long prod = 1;
    for (Obj *args = eval_list(root, env, list); args != Nil; args = args->val.cell.cdr) {
        if (args->val.cell.car->type != TLONG) {
            error("%s takes only numbers\n", "*");
        }
        prod *= args->val.cell.car->val.value;
    }
    return make_long(root, prod);
}


// (- <integer> ...)
static Obj *prim_minus(void *root, Obj **env, Obj **list) {
    Obj *args = eval_list(root, env, list);
    for (Obj *p = args; p != Nil; p = p->val.cell.cdr)
        if (p->val.cell.car->type != TLONG) {
            error("%s takes only numbers\n", "-");
        }
    if (args->val.cell.cdr == Nil)
        return make_long(root, -args->val.cell.car->val.value);
    long r = args->val.cell.car->val.value;
    for (Obj *p = args->val.cell.cdr; p != Nil; p = p->val.cell.cdr)
        r -= p->val.cell.car->val.value;
    return make_long(root, r);
}

// (< <integer> <integer>)
static Obj *prim_lt(void *root, Obj **env, Obj **list) {
    Obj *args = eval_list(root, env, list);
    if (length(args) != 2) {
        error("Malformed %s\n", "<");
    }
    Obj *x = args->val.cell.car;
    Obj *y = args->val.cell.cdr->val.cell.car;
    if (x->type != TLONG || y->type != TLONG) {
        error("%s takes only numbers\n", "<");
    }
    return x->val.value < y->val.value ? True : Nil;
}

static Obj *handle_function(void *root, Obj **env, Obj **list, int type) {
    if ((*list)->type != TCELL || !is_list((*list)->val.cell.car) || (*list)->val.cell.cdr->type != TCELL) {
        error("Malformed %s\n", "lambda");
    }
    Obj *p = (*list)->val.cell.car;
    for (; p->type == TCELL; p = p->val.cell.cdr)
        if (p->val.cell.car->type != TSYMBOL) {
            error("Parameter must be a symbol\n");
        }
    if (p != Nil && p->type != TSYMBOL) {
        error("Parameter must be a symbol\n");
    }
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
    if ((*list)->val.cell.car->type != TSYMBOL || (*list)->val.cell.cdr->type != TCELL) {
        error("Malformed %s\n", "defun");
    }
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
    if (length(*list) != 2 || (*list)->val.cell.car->type != TSYMBOL) {
        error("Malformed %s\n", "define");
    }
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
    if (length(*list) != 1) {
        error("Malformed %s\n", "macroexpand");
    }
    DEFINE1(body);
    *body = (*list)->val.cell.car;
    return macroexpand(root, env, body);
}

// (println expr)
static Obj *prim_println(void *root, Obj **env, Obj **list) {
    DEFINE1(tmp);
    *tmp = (*list)->val.cell.car;
    print(eval(root, env, tmp));
    bprintf("\n");
    return Nil;
}

// (if expr expr expr ...)
static Obj *prim_if(void *root, Obj **env, Obj **list) {
    if (length(*list) < 2) {
        error("Malformed %s\n", "if");
    }
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

typedef enum NUM_CMP_OPT {
    NUM_CMP_OPT_EQ = 0,
    NUM_CMP_OPT_GT,
    NUM_CMP_OPT_LT,
    NUM_CMP_OPT_GTE,
    NUM_CMP_OPT_LTE
} NUM_CMP_OPT;

static Obj *prim_num_cmp(void *root, Obj **env, Obj **list, NUM_CMP_OPT opt, const char *label) {
    if (length(*list) != 2) {
        error("Malformed %s\n", "=");
    }

    Obj *values = eval_list(root, env, list);
    Obj *x = values->val.cell.car;
    Obj *y = values->val.cell.cdr->val.cell.car;
    if (x->type != TLONG || y->type != TLONG)
        error("%s only takes numbers\n", label);
    long v1 = x->val.value;
    long v2 = y->val.value;

    bool result;
    switch(opt) {
        case NUM_CMP_OPT_EQ:
            result = (v1 == v2);
            break;
        case NUM_CMP_OPT_GT:
            result = (v1 > v2);
            break;
        case NUM_CMP_OPT_LT:
            result = (v1 < v2);
            break;
        case NUM_CMP_OPT_GTE:
            result = (v1 >= v2);
            break;
        case NUM_CMP_OPT_LTE:
            result = (v1 <= v2);
            break;
        default:
            result = FALSE;
  }
  return result ? True : Nil;
}

// (= <integer> <integer>)
static Obj *prim_num_eq(void *root, Obj **env, Obj **list) {
    return prim_num_cmp(root, env, list, NUM_CMP_OPT_EQ, "=");
}

// (< <integer> <integer>)
static Obj *prim_num_lt(void *root, Obj **env, Obj **list) {
    return prim_num_cmp(root, env, list, NUM_CMP_OPT_LT, "<");
}

// (> <integer> <integer>)
static Obj *prim_num_gt(void *root, Obj **env, Obj **list) {
    return prim_num_cmp(root, env, list, NUM_CMP_OPT_GT, ">");
}

// (<= <integer> <integer>)
static Obj *prim_num_lte(void *root, Obj **env, Obj **list) {
    return prim_num_cmp(root, env, list, NUM_CMP_OPT_LTE, "<=");
}

// (>= <integer> <integer>)
static Obj *prim_num_gte(void *root, Obj **env, Obj **list) {
    return prim_num_cmp(root, env, list, NUM_CMP_OPT_GTE, ">=");
}

// (eq expr expr)
static Obj *prim_eq(void *root, Obj **env, Obj **list) {
    if (length(*list) != 2) {
        error("Malformed %s\n", "eq");
    }
    Obj *values = eval_list(root, env, list);
    return values->val.cell.car == values->val.cell.cdr->val.cell.car ? True : Nil;
}

static void add_primitive(void *root, Obj **env, const char *name, Primitive *fn) {
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
    Obj *args = eval_list(root, env, list);
    if (args->val.cell.car->type != TSYMBOL) {
        error("Malformed %s\n", "load");
    }

    if (doing_load)
      return Nil;

    char filebuf[13];
    strncpy(filebuf, args->val.cell.car->val.name, 8);
    for(byte ii=0; ii<8; ii++)
      filebuf[ii] = (char)toupper(filebuf[ii]);
    strcat(filebuf, ".LSP");
    if (bopen(&fd, filebuf)) {
      return True;
    } else {
      return Nil;
    }
}


static void define_primitives(void *root, Obj **env) {
    add_primitive(root, env, "quote", (void **)prim_quote);
    add_primitive(root, env, "cons", (void **)prim_cons);
    add_primitive(root, env, "car", (void **)prim_car);
    add_primitive(root, env, "cdr", (void **)prim_cdr);
    add_primitive(root, env, "setq", (void **)prim_setq);
    add_primitive(root, env, "setcar", (void **)prim_setcar);
    add_primitive(root, env, "while", (void **)prim_while);
    add_primitive(root, env, "gensym", (void **)prim_gensym);
    add_primitive(root, env, "+", (void **)prim_plus);
    add_primitive(root, env, "*", (void **)prim_mult);
    add_primitive(root, env, "-", (void **)prim_minus);
    add_primitive(root, env, "<", (void **)prim_lt);
    add_primitive(root, env, "define", (void **)prim_define);
    add_primitive(root, env, "defun", (void **)prim_defun);
    add_primitive(root, env, "defmacro", (void **)prim_defmacro);
    add_primitive(root, env, "macroexpand", (void **)prim_macroexpand);
    add_primitive(root, env, "lambda", (void **)prim_lambda);
    add_primitive(root, env, "if", (void **)prim_if);
    add_primitive(root, env, "=", (void **)prim_num_eq);
    add_primitive(root, env, "<", (void **)prim_num_lt);
    add_primitive(root, env, ">", (void **)prim_num_gt);
    add_primitive(root, env, "<=", (void **)prim_num_lte);
    add_primitive(root, env, ">=", (void **)prim_num_gte);
    add_primitive(root, env, "eq", (void **)prim_eq);
    add_primitive(root, env, "println", (void **)prim_println);
    add_primitive(root, env, "load", (void **)prim_load);
}

//======================================================================
// Entry point
//======================================================================

// Returns true if the environment variable is defined and not the empty string.
static bool getEnvFlag(char *name) {
    return false;
}


static void exit_if_broken_mame() {
    long val1 = 0;
    long val2 = -1;
    long *v1 = &val1;
    long *v2 = &val2;

    if (*v1 <= *v2) {
      printf("REQUIRES MAME 0.191 OR LATER");
      exit(0);
    }
}

// sbrk will not work when we clear memory so we allocate a local fat buffer
extern byte *fatBuffer;
byte localFatBuffer[68];

int main() {
    initCoCoSupport();
    fatBuffer = localFatBuffer; // init fatBuffer to avoid sbrk
    exit_if_broken_mame();
    setHighSpeed(TRUE);

    swap_in_basic_for_print();
    width(SCREEN_WIDTH);
    swap_out_basic_after_print();
    bprintf(VERSION);
    bprintf("Original by Rui Ueyama\n");
    bprintf("CoCo port: Jamie Cho\n\n");
    bprintf("Press <BREAK> to eval commands\n\n");

    // Force lowercase mode
    asm { clr 282 }

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
    swap_out_basic_after_print();
    has_file_data = FALSE;
    if (doing_load) {
      bclose(&fd);
    }
    for (;;) {
        *expr = read_expr(root);
        if (!*expr)
            continue;
        if (*expr == Cparen)
            error("Stray close parenthesis\n");
        if (*expr == Dot)
            error("Stray dot\n");
        print(eval(root, env, expr));
        bprintf("\n");
    }

    return 0;
}
