#ifndef array_h
#define array_h

#include "rc.h"
#include "type.h"

typedef struct {
  size_t len;
  Rc data;
} Array;

Array Array_alloc(const Type* a, size_t len);

extern const Type1 Type1_array;

#endif // array_h
