#include <alloca.h>
#include <string.h>
#include <sys/param.h>
#include <stdio.h>
#include <stddef.h>
#include <stdalign.h>
#include <stdbool.h>
#include <string.h>

#include "builtins.h"
#include "runtime/closure.h"
#include "runtime/rc.h"
#include "runtime/type.h"

// Option : Type -> Type
typedef enum {
  Some,
  None
} OptionTag;

const OptionTag* Option_tag_const(const void* value) {
  return (const OptionTag*)value;
}

OptionTag* Option_tag(void* value) {
  return (OptionTag*)Option_tag_const(value);
}

const void* Option_Some_value_const(const Type* a, const void* value) {
  return
    value +
    // tag
    sizeof(size_t) +
    // tag to value padding
    (a->alignment - sizeof(size_t) % a->alignment)
  ;
}

void* Option_Some_value(const Type* a, void* value) {
  return (void*)Option_Some_value_const(a, value);
}

typedef struct {
  size_t size;
  size_t alignment;
  FP_Type_copy copy;
  FP_Type_move move;

  // Internal
  const Type* a;
} Type_Option;

void Type_Option_copy(Type_Option* self, void* dest, const void* src) {
  switch (*Option_tag_const(src)) {
    case Some:
      *Option_tag(dest) = Some;
      Type_copy(
        self->a,
        Option_Some_value(self->a, dest),
        Option_Some_value_const(self->a, src)
      );
      break;
    case None: {
      *Option_tag(dest) = None;
      break;    
    }
  }
}

void Type_Option_move(Type_Option* self, void* dest, const void* src) {
  memcpy(dest, src, self->size);
}

void Type_Option_make(Type_Option* result, const Type* a) {
  result->size =
    // tag
    sizeof(size_t) +
    // tag to value padding
    (a->alignment - sizeof(size_t) % a->alignment) +
    // value
    a->size;

  result->alignment = MAX(
    alignof(size_t),
    a->alignment
  );

  result->copy = (FP_Type_copy)&Type_Option_copy;
  result->move = (FP_Type_move)&Type_Option_move;
}

Rc TypeCon_Option_apply(const TypeCon* self, const Type* arg) {
  Rc ptr = Rc_alloc(alignof(Type_Option), sizeof(Type_Option));
  
  Type_Option_make(Rc_data(ptr), arg);
  
  return ptr;
}
const TypeCon TypeCon_Option = { .apply = (FP_TypeCon_apply)&TypeCon_Option_apply };

void Option_Some(void* result, const Type* a, const void* arg) {
  *((OptionTag*)result) = Some;
  Type_move(a, Option_Some_value(a, result), arg);
}

void Option_None(void* result, const Type* a) {
  *((OptionTag*)result) = None;
}

// data Result (a b : Type) : Type = Err(a) | Ok(b)
typedef enum {
  Ok,
  Err
} ResultTag;

const ResultTag* Result_tag_const(const void* value) {
  return (const ResultTag*)value;
}

ResultTag* Result_tag(void* value) {
  return (ResultTag*)Result_tag_const(value);
}

const void* Result_Ok_value_const(const Type* a, const Type* b, const void* value) {
  return
    value +
    // tag
    sizeof(size_t) +
    // tag to value padding
    (b->alignment - sizeof(size_t) % b->alignment)
  ;
}

void* Result_Ok_value(const Type* a, const Type* b, void* value) {
  return (void*)Result_Ok_value_const(a, b, value);
}

const void* Result_Err_value_const(const Type* a, const Type* b, const void* value) {
  return
    value +
    // tag
    sizeof(size_t) +
    // tag to value padding
    (a->alignment - sizeof(size_t) % a->alignment)
  ;
}

void* Result_Err_value(const Type* a, const Type* b, void* value) {
  return (void*)Result_Err_value_const(a, b, value);
}

typedef struct {
  size_t size;
  size_t alignment;
  FP_Type_copy copy;
  FP_Type_move move;

  // Internal
  const Type* a;
  const Type* b;
} Type_Result;

void Type_Result_copy(Type_Result* self, void* dest, const void* src) {
  switch (*Result_tag_const(src)) {
    case Err: {
      *Result_tag(dest) = Err;
      Type_copy(
        self->a,
        Result_Err_value(self->a, self->b, dest),
        Result_Err_value_const(self->a, self->b, src)
      );
      break;    
    }
    case Ok:
      *Result_tag(dest) = Ok;
      Type_copy(
        self->b,
        Result_Ok_value(self->a, self->b, dest),
        Result_Ok_value_const(self->a, self->b, src)
      );
      break;
  }
}

void Type_Result_move(Type_Option* self, void* dest, const void* src) {
  memcpy(dest, src, self->size);
}

void Type_Result_make(Type_Result* result, const Type* a, const Type* b) {
  result->size =
    // tag
    sizeof(size_t) +
    MAX(
      // tag to value padding
      (sizeof(size_t) - (sizeof(size_t) % a->alignment)) +
      // value
      a->size
      ,
      // tag to value padding
      (sizeof(size_t) - (sizeof(size_t) % b->alignment)) +
      // value
      b->size
    );

  result->alignment = MAX(
    alignof(size_t),
    MAX(
      a->alignment,
      b->alignment
    )
  );

  result->copy = (FP_Type_copy)&Type_Result_copy;
  result->move = (FP_Type_move)&Type_Result_move;

  result->a = a;
  result->b = b;
}

Rc TypeCon_Result_a_apply(TypeCon_partial* self, const Type* arg) {
  Rc ptr = Rc_alloc(alignof(Type_Result), sizeof(Type_Result));
  
  Type_Result_make(Rc_data(ptr), self->arg, arg);
  
  return ptr;
}

Rc TypeCon_Result_apply(TypeCon* self, const Type* arg) {
  Rc ptr = Rc_alloc(alignof(TypeCon_partial), sizeof(TypeCon_partial));

  ((TypeCon_partial*)Rc_data(ptr))->apply = (FP_TypeCon_apply)&TypeCon_Result_a_apply;
  ((TypeCon_partial*)Rc_data(ptr))->arg = (void*)arg;

  return ptr;
}

const TypeCon TypeCon_Result = { .apply = (FP_TypeCon_apply)&TypeCon_Result_apply };


typedef struct {
  size_t len;
  Rc data;
} Array_any;

// Array : Type -> Type
void Array_Type_copy(const Type* self, Array_any* dest, Array_any* src) {
  dest->len = src->len;
  dest->data = Rc_copy(src->data);
}

void Array_Type_move(const Type* self, Array_any* dest, Array_any* src) {
  memcpy(dest, src, self->size);
}

void Array_TypeCon_type_con(Type* result, const Type* a) {
  result->size = sizeof(Array_any);
  result->alignment = alignof(Array_any);
}

void Array_TypeCon_apply(Type* result, void* self, const Type* a) {
  Array_TypeCon_type_con(result, a);
}

const TypeCon Array_TypeCon = {
  .apply = (FP_TypeCon_apply)Array_TypeCon_apply
};

Array_any Array_alloc(const Type* a, size_t len) {
  Array_any result = {
    .len = len,
    .data = Rc_alloc(a->alignment, len * a->size)
  };
  return result;
}

void Array_i32_debug(Array_any array) {
  printf("[");
  if (array.len > 0) {
    printf("%d", *(int32_t*)Rc_data(array.data));
    if (array.len > 1) {
      for (size_t i = 1; i < array.len; i++) {
        printf(", %d", *(int32_t*)((char*)Rc_data(array.data) + i * Type_i32.size));
      }
    }
  }
  printf("]\n");
}

void Array_free(Array_any array) {
  Rc_free(array.data);
}


/*
class Functor f where
  fmap : (a b : Type, a -> b, f a) -> f b
*/
typedef struct {
  void (*fmap)(void* result, const Type* a, const Type* b, const Closure* f, void* f_a);
} Functor;


/*
instance Functor Option where
  fmap(a b : Type, f : a -> b, x : Option a) : Option b =
    case x of
      Some a -> Some(f a)
      None -> None
*/
void Functor_Option_fmap(void* result, const Type* a, const Type* b, const Closure* f, void* x) {
  /*
  `Option a` has the following layout:

  ```
  | tag (8 bytes) | layout(a) (sizeof(a) bytes) |
  ```

  The argument of `Some` is stored contiguously, never boxed.

  Therefore a function taking an `Option a` must do so by reference. `Option a` is
  dynamically-sized because its size is determined by the size of the `a`.
  */
  switch (*Option_tag(x)) {
    case Some: {      
      *Option_tag(result) = Some;
      Closure_applyPP(Option_Some_value(b, result), f, Option_Some_value(a, x));      
      break;    
    }
    case None:
      *Option_tag(result) = None;
      break;
  }
}
const Functor Functor_Option = {
  .fmap = &Functor_Option_fmap
};


/*
instance Functor Array where
  fmap(a b : Type, f : a -> b, x : Array a) : Array b = ...
*/
void Functor_Array_fmap(void* result, const Type* a, const Type* b, const Closure* f, void* x) {
  size_t len = ((Array_any*)x)->len;
  Rc input_data = ((Array_any*)x)->data;
  Rc output_data = Rc_alloc(b->alignment, len * b->size);

  for (size_t offset = 0; offset < b->size * len; offset += b->size) {
    Closure_applyPP(
      (char*)Rc_data(output_data) + offset,
      f,
      (char*)Rc_data(input_data) + offset
    );
  }
  
  ((Array_any*)result)->len = len;
  ((Array_any*)result)->data = output_data;
}
const Functor Functor_Array = {
  .fmap = &Functor_Array_fmap
};


// \(x : i32) -> x + n
typedef struct {
  FP_Closure_apply apply;
  int32_t n;
} Closure_x_arr_x_plus_n;

int32_t Closure_x_arr_x_plus_n_code(const Closure_x_arr_x_plus_n* self, int32_t arg) {
  return arg + self->n;  
}

// {a=i32, b=i32} |- (\(x : i32) -> x + n) : a -> b
void Closure_x_arr_x_plus_n_code_wrapper(void* result, const Closure_x_arr_x_plus_n* self, void* arg) {
  *((int32_t*)result) = Closure_x_arr_x_plus_n_code(self, *((int32_t*)arg));
}


void fmap_main() {
  printf("fmap:\n");

  // Result i32 : Type -> Type
  Rc Result_i32 = TypeCon_apply(&TypeCon_Result, (void*)&Type_i32);
  // Result i32 bool : Type
  Rc Result_i32_bool = TypeCon_apply((TypeCon*)Rc_data(Result_i32), (void*)&Type_bool);
  printf("sizeOf(i32) = %ld\n", Type_i32.size);
  printf("sizeOf(bool) = %ld\n", Type_bool.size);
  printf("sizeOf(Result i32 bool) = %ld\n", ((Type*)Rc_data(Result_i32_bool))->size);

  
  Rc Option_i32 = TypeCon_apply(&TypeCon_Option, (void*)&Type_i32);
  
  // let x : Option i32 = Some(22) in
  void* x = alloca(((Type*)Rc_data(Option_i32))->size);
  *Option_tag(x) = Some;
  *(int32_t*)Option_Some_value(&Type_i32, x) = 22;
  
  // let n : i32 = 3 in
  int32_t n = 3;
  
  // let y : Option i32 = fmap (\x -> x + n) x in
  Closure_x_arr_x_plus_n* arg = alloca(sizeof(Closure_x_arr_x_plus_n));
  arg->apply = (FP_Closure_apply)&Closure_x_arr_x_plus_n_code_wrapper;
  arg->n = n;
  void* y = alloca(((Type*)Rc_data(Option_i32))->size); Functor_Option.fmap(y, &Type_i32, &Type_i32, (Closure*)arg, x);
  
  switch (*Option_tag(y)) {
    case Some: {
      printf("Some(%d)\n", *(int32_t*)Option_Some_value(&Type_i32, y));
      break;
    }
    case None: {
      printf("None()\n");
      break;
    }
  }


  // let xs : Array i32 = [1, 2, 3, 4, 5]
  Array_any xs = Array_alloc(&Type_i32, 5);
  for (size_t i = 0; i < xs.len; i++) {
    *(int32_t*)((char*)Rc_data(xs.data) + i*Type_i32.size) = i + 1;
  }
  Array_i32_debug(xs);

  // let ys : Array i32 = fmap (\x -> x + n) xs in
  Array_any ys; Functor_Array.fmap(&ys, &Type_i32, &Type_i32, (Closure*)arg, &xs);
  Array_free(xs);
  
  Array_i32_debug(ys);
  Array_free(ys);
}
