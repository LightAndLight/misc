#include <alloca.h>
#include "runtime/array.h"
#include "runtime/closure.h"
#include "runtime/rc.h"
#include "runtime/type.h"

static inline __attribute__((always_inline)) void* aligned_alloca(size_t alignment, size_t size) {
  // https://stackoverflow.com/a/46879080/2884502
  void* ptr = alloca(size + alignment - 1);
  return (void*)(((size_t)ptr + (alignment - 1)) & ~(alignment - 1));
}

