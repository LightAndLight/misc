#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include "builtins.h"

// fn const(a b : Type, x : a, y : b) : a = x
void fn_const(void* result, const Type* a, const Type* b, const void* x, const void* y) {
  Type_move(a, result, x);
}

// fn dup(a : Type, x : a) : (a, a) = (x, x)
void dup(void* result, const Type* a, const void* x) {
  Type_copy(a, T2_fst(a, a, result), x);
  Type_move(a, T2_snd(a, a, result), x);
}

void simple_main() {
  printf("simple:\n");
  
  // let a : i32 = 22
  int32_t a = 22;

  // let b : bool = true
  bool b = true;

  // let c : i32 = const(i32, bool, a, b);
  int32_t c; fn_const(&c, &Type_i32, &Type_bool, &a, &b);

  printf("%d\n", c);
}
