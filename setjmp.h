#ifndef _setjmp_h
#define _setjmp_h

#include <coco.h>

struct jmp_buf {
  unsigned pc, s, u;
};

typedef struct jmp_buf jmp_buf;

int setjmp(jmp_buf *env);
void longjmp(jmp_buf *env, int value);

#endif
