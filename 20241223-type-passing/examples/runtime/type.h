#ifndef type_h
#define type_h

#include <stddef.h>

#include "rc.h"

typedef void(*FP_Type_copy)(const void* self, void* dest, const void* src);
typedef void(*FP_Type_move)(const void* self, void* dest, const void* src);

/* Runtime representation of type with kind `Type`.
*
* Formally: `|- a : Type`.
*/
typedef struct {
  size_t size;
  size_t alignment;
  FP_Type_copy copy;
  FP_Type_move move;
} Type;

// Copy data from `src` to `dest`.
void Type_copy(const Type* self, void* dest, const void* src);
 
/* Move data from `src` to `dest`.
*
* Ownership of the data is transferred from `src` to `dest`.
*/
void Type_move(const Type* self, void* dest, const void* src);

/* A `Type` with a single dependency.
*
* Formally: `a : k |- b : Type`.
*/
typedef size_t (*FP_Type1_size)(const void* a);
typedef size_t (*FP_Type1_alignment)(const void* a);
typedef void (*FP_Type1_copy)(const void* a, const void* self, void* dest, const void* src);
typedef void (*FP_Type1_move)(const void* a, const void* self, void* dest, const void* src);
typedef struct {
  FP_Type1_size size;
  FP_Type1_alignment alignment;
  FP_Type1_copy copy;
  FP_Type1_move move;
} Type1;

size_t Type1_size(const Type1* self, const void* a);
size_t Type1_alignment(const Type1* self, const void* a);
void Type1_copy(const Type1* self, const void* a, void* dest, const void* src);
void Type1_move(const Type1* self, const void* a, void* dest, const void* src);

typedef Rc (*FP_TypeCon_apply)(const void* self, const void* arg);

// Runtime representation type constructor (types with kind `Type -> k`).
typedef struct {
  FP_TypeCon_apply apply;
} TypeCon;

Rc TypeCon_apply(const TypeCon* self, const void* arg);

// A partially-applied type constructor.
typedef struct {
  FP_TypeCon_apply apply;

  // Internal
  const void* arg;
} TypeCon_partial;

#endif // type_h
