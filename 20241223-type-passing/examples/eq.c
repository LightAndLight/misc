#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include "builtins.h"
#include "runtime/type.h"

/*
type Eq a = {
  eq : (a, a) -> bool
}
*/
typedef struct {
  bool (*eq)(void* x, void* y);
} Eq;

// fn eq(a : Type, dict : Eq a, x y : a) : bool = dict.eq(x, y)
bool fn_eq(const Type* a, const Eq* dict, void* x, void* y) {
  return dict->eq(x, y);
}

// fn swap_eq(a : Type, dict : Eq a, x y : a) : bool = eq(a, dict, y, x)
bool fn_swap_eq(const Type* a, const Eq* dict, void* x, void* y) {
  return fn_eq(a, dict, y, x);
}

/*
val Eq_i32 : Eq i32 = {
  eq = \(x, y) -> eq_i32(x, y)
}
*/
bool eq_i32(void* x, void* y) {
  return *((int32_t*)x) == *((int32_t*)y);
}
const Eq Eq_i32 = {
  .eq = &eq_i32
};

void eq_main() {
  printf("eq:\n");
  {
    // let x = eq(i32, Eq_i32, 99, 99)
    int32_t arg_0 = 99;
    int32_t arg_1 = 99;
    bool x = fn_eq(&Type_i32, &Eq_i32, &arg_0, &arg_1);
  
    if (x) {
      printf("true\n");
    } else {
      printf("false\n");
    };
  }

  {
    // let x = eq(i32, Eq_i32, 23, 65)
    int32_t arg_0 = 23;
    int32_t arg_1 = 65;
    bool x = fn_eq(&Type_i32, &Eq_i32, &arg_0, &arg_1);
  
    if (x) {
      printf("true\n");
    } else {
      printf("false\n");
    };
  }  
}
