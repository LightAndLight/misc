#include <stdalign.h>
#include <stdlib.h>
#include <stddef.h>
#include <sys/param.h>

#include "rc.h"

typedef struct { size_t alignment; size_t size; size_t count; } Metadata;
typedef struct Object_t { Metadata meta; /* data */ } Object;

Rc Rc_alloc(size_t alignment, size_t size) {
  Object* ptr = (Object*)aligned_alloc(
    MAX(alignof(Metadata), alignment),
    // metadata
    sizeof(Metadata) +
    // metadata to data padding
    (alignment - sizeof(Metadata) % alignment) +
    // data
    size
  );
  ptr->meta.alignment = alignment;
  ptr->meta.size = size;
  ptr->meta.count = 1;
  
  Rc rc = {
    .value = ptr
  };
  return rc;
}

void Rc_free(Rc ptr) {
  if (ptr.value->meta.count <= 1) {
    free(ptr.value);
  } else {
    ptr.value->meta.count -= 1;
  }
}

Rc Rc_copy(Rc ptr) {
  ptr.value->meta.count += 1;
  return ptr;
}

void* Rc_data(Rc ptr) {
  size_t alignment = ptr.value->meta.alignment;
  char* data =
    (char*)ptr.value +
    sizeof(Metadata) +
    (alignment - sizeof(Metadata) % alignment)
  ;
  return (void*)data;
}
