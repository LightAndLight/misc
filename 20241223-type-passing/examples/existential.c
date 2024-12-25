#include <alloca.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/param.h>

#include "builtins.h"
#include "runtime/closure.h"
#include "runtime/rc.h"
#include "runtime/type.h"

// (\x -> x + x) : i64 -> i64
typedef struct {
  FP_Closure_apply apply;
} Closure_existential0;
int64_t Closure_existential0_code(Closure_existential0* self, int64_t x) {
  return x + x;
}
// {s=i64} |- (\x -> x + x) : s -> s
void Closure_existential0_code_wrapped(void* result, Closure_existential0* self, void* arg) {
  *(int64_t*)result = Closure_existential0_code(self, *(int64_t*)arg);
}

// (\x -> x) : i64 -> i64
typedef struct {
  FP_Closure_apply apply;
} Closure_existential1;
int32_t Closure_existential1_code(Closure_existential1* self, int64_t x) {
  return x;
}
// {s=i64} |- (\x -> x) : s -> i32
int32_t Closure_existential1_code_wrapped(Closure_existential1* self, void* arg) {
  return Closure_existential1_code(self, *(int64_t*)arg);
}

// (\x -> x + 1) : i32 -> i32
typedef struct {
  FP_Closure_apply apply;
} Closure_existential3;
int32_t Closure_existential3_code(Closure_existential3* self, int32_t x) {
  return x + 1;
}
// {s=i32} |- (\x -> x + 1) : s -> s
void Closure_existential3_code_wrapped(void* result, Closure_existential3* self, void* arg) {
  *(int32_t*)result = Closure_existential3_code(self, *(int32_t*)arg);
}

// (\x -> x) : i32 -> i32
typedef struct {
  FP_Closure_apply apply;
} Closure_existential4;
int32_t Closure_existential4_code(Closure_existential4* self, int32_t x) {
  return x;
}
// {s=i32} |- (\x -> x) : s -> i32
int32_t Closure_existential4_code_wrapped(Closure_existential4* self, void* arg) {
  return Closure_existential4_code(self, *(int32_t*)arg);
}

/*
Notes on returning existential types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An existential type can't be dynamically sized, because when one is returned from a
function the caller needs to know how much memory to allocate for the result.

For `id : (a : Type) -> a -> a`, the caller knows the size of `a` so it can allocate stack
space for the `a` returned by `id x`.

This is not the case for `not_id : (a : Type) -> a -> { s : Type, x : s }`. Callers of
`not_id` need to behave identically for `not_id a x = { s = a, x = x }` and
`not_id _ _ = { s = i32, x = 0 }`. If `{ s : Type, x : s }` were dynamically sized, then
its size would be determined by the size of `s`, which is not known when the function is
called.

`{ s : Type, x : s }` has to be statically sized. There are two obvious ways to do it:

1. give `x : s` pointer representation
2. give the entire record pointer representation

(2) seems better. We've already established that values with variable types are represented
by pointers (for example, the `x` in `id : (a : Type) -> (x : a) -> a`). Choosing (2) means
that given `value : { s : Type, x : s }`, `value.x : value.s` is simply a pointer that's
been offset from `value` (which is also a pointer).

Given that a sigma type is represented by a pointer, the next question is: to where should that
pointer points? In general it can't be a stack pointer, because the sigma type may be returned from
a function. So the sigma type must be represented by a heap pointer. In other words, sigma
types are boxed.

Upon reflection, there's a third representation that's more true to the type theory:

3. A(n unboxed) pair of static component `s : Type` and boxed dynamic component `x : s`.

`{ s : Type, x : s }` is isomorphic to `Sigma Type (\s -> s)`, a dependent pair. Our more
complex example `{ s : Type, state : s, incr : s -> s, read : s -> i32 }` is similarly
isomorphic to `Sigma Type (\s -> { state : s, incr : s -> s, read : s -> i32 })`.

`x : Sigma A B |- fst x : A` is unboxed because `A` is a statically known value.
`x : Sigma A B |- snd x : B (fst x)` is boxed because `B (fst x)` is a dependent type.
```
*/

/*
{ s : Type, state : s, incr : s -> s, read : s -> i32 }

is isomorphic to

Sigma Type (\s -> { state : s, incr : s -> s, read : s -> i32 })
*/
void Type_Sigma_existential_c_1_copy(const Type* self, Rc* dest, Rc* src) {
  *dest = Rc_copy(*src);
}

void Type_Sigma_existential_c_1_move(const Type* self, Rc* dest, Rc* src) {
  *dest = *src;
}

const Type Type_Sigma_existential_c_1 = {
  .size = sizeof(Rc),
  .alignment = alignof(Rc),
  .copy = (FP_Type_copy)&Type_Sigma_existential_c_1_copy,
  .move = (FP_Type_move)&Type_Sigma_existential_c_1_move
};

// s : Type |- { state : s, incr : s -> s, read : s -> i32 }
typedef struct {
  size_t size;
  size_t alignment;
  FP_Type_copy copy;
  FP_Type_move move;

  // Internal
  const Type* a;
} Type_exists_rhs_existential_c_1;

/*
s : Type |- x : { state : s, incr : s -> s, read : s -> i32 })
--------------------------------------------------------------
                  s : Type |- x.state : s
*/
void* Type1_exists_rhs_existential_c_1_state(const Type* s, void* record) {
  return record;
}

/*
s : Type |- x : { state : s, incr : s -> s, read : s -> i32 })
--------------------------------------------------------------
                  s : Type |- x.incr : s -> s
*/
Rc* Type1_exists_rhs_existential_c_1_incr(const Type* s, void* record) {
  return (Rc*)(
    (char*)Type1_exists_rhs_existential_c_1_state(s, record) +
    // state
    s->size +
    // state to incr padding
    (alignof(Rc) - s->size % alignof(Rc))
  );
}

/*
s : Type |- x : { state : s, incr : s -> s, read : s -> i32 })
--------------------------------------------------------------
                  s : Type |- x.read : s -> i32
*/
Rc* Type1_exists_rhs_existential_c_1_read(const Type* s, void* record) {
  return (Rc*)(
    (char*)Type1_exists_rhs_existential_c_1_incr(s, record) +
    // incr
    sizeof(Rc) +
    // incr to read padding
    (alignof(Rc) - sizeof(Rc) % alignof(Rc))
  );
}

size_t Type1_exists_rhs_existential_c_1_size(const Type* a) {
  return
    // state
    a->size +
    // state to incr padding
    (alignof(Rc) - a->size % alignof(Rc)) +
    // incr
    sizeof(Rc) +
    // incr to read padding
    (alignof(Rc) - sizeof(Rc) % alignof(Rc)) +
    // read
    sizeof(Rc)
  ;
}

size_t Type1_exists_rhs_existential_c_1_alignment(const Type* a) {
  return MAX(
    // state
    a->alignment,
    MAX(
      // incr
      alignof(Rc),
      // read
      alignof(Rc)
    )
  );
}

void Type1_exists_rhs_existential_c_1_copy(const Type* a, const Type1* self, void* dest, void* src) {
  Type_copy(
    a,
    Type1_exists_rhs_existential_c_1_state(a, dest),
    Type1_exists_rhs_existential_c_1_state(a, src)
  );
  Type_copy(
    &Type_arrow,
    Type1_exists_rhs_existential_c_1_incr(a, dest),
    Type1_exists_rhs_existential_c_1_incr(a, src)
  );
  Type_copy(
    &Type_arrow,
    Type1_exists_rhs_existential_c_1_read(a, dest),
    Type1_exists_rhs_existential_c_1_read(a, src)
  );
}

void Type1_exists_rhs_existential_c_1_move(const Type* a, const Type1* self, void* dest, void* src) {
  memcpy(dest, src, self->size(a));
}

Type1 Type1_exists_rhs_existential_c_1 = {
  .size = (FP_Type1_size)&Type1_exists_rhs_existential_c_1_size,
  .alignment = (FP_Type1_alignment)&Type1_exists_rhs_existential_c_1_alignment,
  .copy = (FP_Type1_copy)&Type1_exists_rhs_existential_c_1_copy,
  .move = (FP_Type1_move)&Type1_exists_rhs_existential_c_1_move
};

/*
make_counter (doubling : bool) : { s : Type, state : s, incr : s -> s, read : s -> i32 } =
  if doubling
  then { s = i64, state = 1, incr = \x -> x + x, read = \x -> x }
  else { s = i32, state = 0, incr = \x -> x + 1, read = \x -> x }
*/
Exists make_counter(bool doubling) {
  Exists result;
  if (doubling) {
    // init fst
    result.fst = &Type_i64;

    // init snd
    Rc tmp = Rc_alloc(
      Type1_alignment(&Type1_exists_rhs_existential_c_1, &Type_i64),
      Type1_size(&Type1_exists_rhs_existential_c_1, &Type_i64)
    );
    {
      void* tmp_data = Rc_data(tmp);
      
      *(int64_t*)Type1_exists_rhs_existential_c_1_state(&Type_i64, tmp_data) = 1;
    
      Rc incr_ptr = Rc_alloc(alignof(Closure_existential0), sizeof(Closure_existential0));
      ((Closure_existential0*)Rc_data(incr_ptr))->apply = (FP_Closure_apply)&Closure_existential0_code_wrapped;
      *Type1_exists_rhs_existential_c_1_incr(&Type_i64, tmp_data) = incr_ptr;
    
      Rc read_ptr = Rc_alloc(alignof(Closure_existential1), sizeof(Closure_existential1));
      ((Closure_existential1*)Rc_data(read_ptr))->apply = (FP_Closure_apply)&Closure_existential1_code_wrapped;
      *Type1_exists_rhs_existential_c_1_read(&Type_i64, tmp_data) = read_ptr;
    }

    result.snd = tmp;
  } else {
    result.fst = &Type_i32;
    
    Rc tmp = Rc_alloc(
      Type1_alignment(&Type1_exists_rhs_existential_c_1, &Type_i32),
      Type1_size(&Type1_exists_rhs_existential_c_1, &Type_i32)
    );
    {
      void* tmp_data = Rc_data(tmp);

      *(int32_t*)Type1_exists_rhs_existential_c_1_state(&Type_i32, tmp_data) = 0;
    
      Rc incr_ptr = Rc_alloc(alignof(Closure_existential3), sizeof(Closure_existential3));
      ((Closure_existential3*)Rc_data(incr_ptr))->apply = (FP_Closure_apply)&Closure_existential3_code_wrapped;
      *Type1_exists_rhs_existential_c_1_incr(&Type_i32, tmp_data) = incr_ptr;
    
      Rc read_ptr = Rc_alloc(alignof(Closure_existential4), sizeof(Closure_existential4));
      ((Closure_existential4*)Rc_data(read_ptr))->apply = (FP_Closure_apply)&Closure_existential4_code_wrapped;
      *Type1_exists_rhs_existential_c_1_read(&Type_i32, tmp_data) = read_ptr;
    }

    result.snd = tmp;
  }
  return result;
}

/*
test_counter (x : { s : Type, state : s, incr : s -> s, read : s -> i32 }) : i32 =
  x.incr (x.incr x.state)
*/
int32_t test_counter(Exists x) {
    // x.state
    void* x_state = Type1_exists_rhs_existential_c_1_state(x.fst, Rc_data(x.snd));
  
    // x.incr x.state
    Rc x_incr = *Type1_exists_rhs_existential_c_1_incr(x.fst, Rc_data(x.snd));
    void* tmp_0 = alloca(x.fst->size); Closure_applyPP(tmp_0, (Closure*)Rc_data(x_incr), x_state);

    // x.incr (x.incr x.state)
    void* tmp_1 = alloca(x.fst->size); Closure_applyPP(tmp_1, (Closure*)Rc_data(x_incr), tmp_0);

    // let y = x.read (x.incr (x.incr x.state)) in
    Rc x_read = *Type1_exists_rhs_existential_c_1_read(x.fst, Rc_data(x.snd));
    int32_t y = Closure_applyPM(int32_t, (Closure*)Rc_data(x_read), tmp_1);

    return y;
}

void existential_main() {
  printf("existential:\n");

  /*
  let x : { s : Type, state : s, incr : s -> s, read : s -> i32 } = make_counter false in
  let y : i32 = test_counter(x) in
  */
  {  
    Exists x = make_counter(false);
    int32_t y = test_counter(x);
    printf("%d\n", y);
  }
  
  /*
  let x : { s : Type, state : s, incr : s -> s, read : s -> i32 } = make_counter true in
  let y : i32 = test_counter(x) in
  */
  {  
    Exists x = make_counter(true);
    int32_t y = test_counter(x);
    printf("%d\n", y);
  }
}
