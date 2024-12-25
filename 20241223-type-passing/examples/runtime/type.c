#include "rc.h"
#include "type.h"

void Type_copy(const Type* ty, void* dest, const void* src) {
  ty->copy(ty, dest, src);
}

void Type_move(const Type* ty, void* dest, const void* src) {
  ty->move(ty, dest, src);
}

size_t Type1_size(const Type1* self, const void* a) {
  return self->size(a);
}

size_t Type1_alignment(const Type1* self, const void* a) {
  return self->alignment(a);
}

void Type1_copy(const Type1* self, const void* a, void* dest, const void* src) {
  return self->copy(a, self, dest, src);
}

void Type1_move(const Type1* self, const void* a, void* dest, const void* src) {
  return self->move(a, self, dest, src);
}

Rc TypeCon_apply(const TypeCon* self, const void* arg) {
  return self->apply(self, arg);
}
