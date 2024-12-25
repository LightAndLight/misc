#include <stdalign.h>

#include "array.h"
#include "rc.h"
#include "type.h"

Array Array_alloc(const Type* a, size_t len) {
  Array array = {
    .len = len,
    .data = Rc_alloc(a->alignment, a->size)
  };
  return array;
}

size_t Type1_array_size(const Type* a) {
  return sizeof(Array);
}

size_t Type1_array_alignment(const Type* a) {
  return sizeof(Array);
}

void Type1_array_copy(const Type* self, const Type* a, Array* dest, const Array* src) {
  dest->len = src->len;
  dest->data = Rc_copy(src->data);
}

void Type1_array_move(const Type* self, const Type* a, Array* dest, const Array* src) {
  *dest = *src;  
}

void Type1_array_drop(const Type* self, const Type* a, Array* value) {
  size_t count = Rc_count(value->data);
  if (count == 1) {
    for (size_t offset = 0; offset < value->len * a->size; offset += a->size) {
      Type_drop(a, ((char*)Rc_data(value->data)) + offset);
    }
    Rc_free(value->data);
  } else {
    Rc_drop(value->data);
  }
}

const Type1 Type1_array = {
  .size = (FP_Type1_size)&Type1_array_size,
  .alignment = (FP_Type1_alignment)&Type1_array_alignment,
  .copy = (FP_Type1_copy)&Type1_array_copy,
  .move = (FP_Type1_move)&Type1_array_move,
  .drop = (FP_Type1_drop)&Type1_array_drop
};
