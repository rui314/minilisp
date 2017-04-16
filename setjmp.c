#ifndef setjmp_c
#define setjmp_c

#include "setjmp.h"


asm int setjmp(jmp_buf *env) {
  asm {
    ldx 2,s
    ldd ,s
    std ,x 
    sts 2,x 
    stu 4,x 
    clra
    clrb
    rts
  }
}


asm void longjmp(jmp_buf *env, int value) {
  asm {
    ldd 4,s
    ldx 2,s
    lds 2,x
    ldu 4,x
    bne longjmp_rts
    ldb #1
longjmp_rts:
    jmp [,x]
  }
}

#endif
