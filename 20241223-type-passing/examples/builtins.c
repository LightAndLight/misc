#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <sys/param.h>

#include "builtins.h"
#include "runtime/rc.h"
#include "runtime/type.h"

// i32 : Type
void Type_i32_copy(const Type* self, int32_t* dest, const int32_t* src) {
  *dest = *src;
}
void Type_i32_move(const Type* self, int32_t* dest, const int32_t* src) {
  *dest = *src;
}
const Type Type_i32 = {
  .size = sizeof(int32_t),
  .alignment = alignof(int32_t),
  .copy = (FP_Type_copy)&Type_i32_copy,
  .move = (FP_Type_move)&Type_i32_move
};

// i64 : Type
void Type_i64_copy(const Type* self, int64_t* dest, const int64_t* src) {
  *dest = *src;
}
void Type_i64_move(const Type* self, int64_t* dest, const int64_t* src) {
  *dest = *src;
}
const Type Type_i64 = {
  .size = sizeof(int64_t),
  .alignment = alignof(int64_t),
  .copy = (FP_Type_copy)&Type_i64_copy,
  .move = (FP_Type_move)&Type_i64_move
};

// bool : Type
void Type_bool_copy(const Type* self, bool* dest, const bool* src) {
  *dest = *src;
}
void Type_bool_move(const Type* self, bool* dest, const bool* src) {
  *dest = *src;
}
const Type Type_bool = {
  .size = sizeof(bool),
  .alignment = alignof(bool),
  .copy = (FP_Type_copy)&Type_bool_copy,
  .move = (FP_Type_move)&Type_bool_move
};

// a : Type, b : Type |- a -> b : Type
void Type_arrow_copy(const Type* self, Rc* dest, const Rc* src) {
  *dest = Rc_copy(*src);
}
void Type_arrow_move(const Type* self, Rc* dest, const Rc* src) {
  *dest = *src;
}
const Type Type_arrow = {
  .size = sizeof(Rc),
  .alignment = alignof(Rc),
  .copy = (FP_Type_copy)&Type_arrow_copy,
  .move = (FP_Type_move)&Type_arrow_move
};

// (,) : Type -> Type -> Type
void* T2_fst(const Type* a, const Type* b, void* t2) {
  return t2;
}
void* T2_snd(const Type* a, const Type* b, void* t2) {
  return
    (char*)t2 +
    a->size +
    (b->alignment - a->size % b->alignment)
  ;
}
typedef struct {
  size_t size;
  size_t alignment;
  FP_Type_copy copy;
  FP_Type_move move;

  // Internal
  const Type* a;
  const Type* b;
} Type_T2;

void Type_T2_copy(Type_T2* self, void* dest, void* src) {
  Type_copy(self->a, T2_fst(self->a, self->b, dest), T2_fst(self->a, self->b, src));
  Type_copy(self->b, T2_snd(self->a, self->b, dest), T2_snd(self->a, self->b, src));
}

void Type_T2_move(Type_T2* self, void* dest, void* src) {
  Type_move(self->a, T2_fst(self->a, self->b, dest), T2_fst(self->a, self->b, src));
  Type_move(self->b, T2_snd(self->a, self->b, dest), T2_snd(self->a, self->b, src));
}

void Type_T2_make(Type_T2* result, const Type* a, const Type* b) {
  result->size =
    // a
    a->size +
    // a to b padding
    (b->alignment - a->size % b->alignment) +
    // b
    b->size;

  result->alignment = MAX(a->alignment, b->alignment);

  result->copy = (FP_Type_copy)&Type_T2_copy;
  result->move= (FP_Type_move)&Type_T2_move;
}

Rc TypeCon_T2_a_apply(const TypeCon_partial* self, const Type* arg) {
  Rc ptr = Rc_alloc(alignof(Type_T2), sizeof(Type_T2));

  Type_T2_make((Type_T2*)Rc_data(ptr), self->arg, arg);
  
  return ptr;
}

Rc TypeCon_T2_apply(const TypeCon* self, const Type* arg) {
  Rc ptr = Rc_alloc(alignof(TypeCon_partial), sizeof(TypeCon_partial));
  
  ((TypeCon_partial*)Rc_data(ptr))->apply = (FP_TypeCon_apply)&TypeCon_T2_a_apply;
  ((TypeCon_partial*)Rc_data(ptr))->arg = arg;
  
  return ptr;
}

const TypeCon TypeCon_T2 = {
  .apply = (FP_TypeCon_apply)&TypeCon_T2_apply
};

void Type_exists_copy(Type_exists* self, Exists* dest, Exists* src) {
  dest->fst = src->fst;
  dest->snd = Rc_copy(src->snd);
}

void Type_exists_move(Type* self, void* dest, void* src) {
  memcpy(dest, src, self->size);
}

void Type_exists_make(Type_exists* result, const Type* a, const Type1* b) {  
  result->size =
    // fst
    a->size +
    // fst to snd padding
    (alignof(Rc) - a->size % alignof(Rc)) +
    // snd
    sizeof(Rc);

  result->alignment = MAX(a->alignment, alignof(Rc));

  result->copy = (FP_Type_copy)&Type_exists_copy;
  result->move = (FP_Type_move)&Type_exists_move;

  result->a = a;
  result->b = b;
}

Exists Exists_alloc(const Type* a, const Type1* b) {
  Exists result = {
    .fst = a,
    .snd = Rc_alloc(b->alignment(a), b->size(a))
  };
  return result;
}
