#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>

#include "rc.h"
#include "type.h"

size_t Type_size(const Type* ty) {
  return ty->size;
}

size_t Type_alignment(const Type* ty) {
  return ty->alignment;
}

void Type_copy(const Type* ty, void* dest, const void* src) {
  ty->copy(ty, dest, src);
}

void Type_move(const Type* ty, void* dest, const void* src) {
  ty->move(ty, dest, src);
}

void Type_drop(const Type* ty, void* value) {
  ty->drop(ty, value);
}

size_t Type1_size(const Type1* self, const void* a) {
  return self->size(a);
}

size_t Type1_alignment(const Type1* self, const void* a) {
  return self->alignment(a);
}

void Type1_copy(const Type1* self, const void* a, void* dest, const void* src) {
  return self->copy(self, a, dest, src);
}

void Type1_move(const Type1* self, const void* a, void* dest, const void* src) {
  return self->move(self, a, dest, src);
}

void Type1_drop(const Type1* self, const void* a, void* value) {
  return self->drop(self, a, value);
}

Rc TypeCon_apply(const TypeCon* self, const void* arg) {
  return self->apply(self, arg);
}

void Type_unit_copy(const Type* self, struct{}* dest, struct{}* src) {}
void Type_unit_move(const Type* self, struct{}* dest, struct{}* src) {}
void Type_unit_drop(const Type* self, struct{}* value) {}
const Type Type_unit = {
  .size = sizeof(struct{}),
  .alignment = alignof(struct{}),
  .copy = (FP_Type_copy)&Type_unit_copy,
  .move = (FP_Type_move)&Type_unit_move,
  .drop = (FP_Type_drop)&Type_unit_drop
};

void Type_bool_copy(const Type* self, bool* dest, bool* src) {
  *dest = *src;
}
void Type_bool_move(const Type* self, bool* dest, bool* src) {
  *dest = *src;
}
void Type_bool_drop(const Type* self, bool* value) {}
const Type Type_bool = {
  .size = sizeof(bool),
  .alignment = alignof(bool),
  .copy = (FP_Type_copy)&Type_bool_copy,
  .move = (FP_Type_move)&Type_bool_move,
  .drop = (FP_Type_drop)&Type_bool_drop
};

void Type_i32_copy(const Type* self, int32_t* dest, int32_t* src) {
  *dest = *src;
}
void Type_i32_move(const Type* self, int32_t* dest, int32_t* src) {
  *dest = *src;
}
void Type_i32_drop(const Type* self, int32_t* value) {}
const Type Type_i32 = {
  .size = sizeof(int32_t),
  .alignment = alignof(int32_t),
  .copy = (FP_Type_copy)&Type_i32_copy,
  .move = (FP_Type_move)&Type_i32_move,
  .drop = (FP_Type_drop)&Type_i32_drop
};

void Type_fn_copy(const Type* self, Rc* dest, Rc* src) {
  *dest = Rc_copy(*src);
}
void Type_fn_move(const Type* self, Rc* dest, Rc* src) {
  *dest = *src;
}
void Type_fn_drop(const Type* self, Rc* value) {
  Rc_drop(*value);
}
const Type Type_fn = {
  .size = sizeof(Rc),
  .alignment = alignof(Rc),
  .copy = (FP_Type_copy)&Type_fn_copy,
  .move = (FP_Type_move)&Type_fn_move,
  .drop = (FP_Type_drop)&Type_fn_drop
};
