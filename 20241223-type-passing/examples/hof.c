#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "runtime/closure.h"
#include "runtime/rc.h"
#include "runtime/type.h"

// \x -> x * n
typedef struct {
  FP_Closure_apply apply;
  int32_t n;
} Closure_1;
int32_t Closure_1_code(Closure_1* self, int32_t x) {
  return x * self->n;
}

// \x -> x
typedef struct {
  FP_Closure_apply apply;
} Closure_2;
int32_t Closure_2_code(Closure_2* self, int32_t x) {
  return x;
}

/*
test (x : bool) (n : i32) : i32 -> i32 =
  if x
  then \x -> x * n
  else \x -> x * 2
*/
Rc test(bool x, int32_t n) {
  if (x) {
    Rc ptr = Rc_alloc(alignof(Closure_1), sizeof(Closure_1));
    ((Closure_1*)Rc_data(ptr))->apply = (FP_Closure_apply)&Closure_1_code;
    ((Closure_1*)Rc_data(ptr))->n = n;
    return ptr;
  } else {
    Rc ptr = Rc_alloc(alignof(Closure_2), sizeof(Closure_2));
    ((Closure_2*)Rc_data(ptr))->apply = (FP_Closure_apply)&Closure_2_code;
    return ptr;
  }
}

// app (a b : Type) (f : a -> b) (x : a) : b = f x
void app(void* result, const Type* a, const Type* b, Closure* f, void* x) {
  Closure_applyPP(result, f, x);
}

void hof_main() {
  printf("hof:\n");

  // let x = test true 5 8 in
  Rc tmp_0 = test(true, 5);
  int32_t x = Closure_applyMM(int32_t, int32_t, (Closure*)Rc_data(tmp_0), 8);
  Rc_free(tmp_0);
  printf("%d\n", x);
  
  // let g = test false 5 8 in
  Rc tmp_1 = test(false, 5);
  int32_t y = Closure_applyMM(int32_t, int32_t, (Closure*)Rc_data(tmp_1), 8);
  Rc_free(tmp_1);
  printf("%d\n", y);
}
