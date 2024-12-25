#ifndef type_h
#define type_h

#include <stddef.h>

#include "rc.h"

typedef void(*FP_Type_copy)(const void* self, void* dest, const void* src);
typedef void(*FP_Type_move)(const void* self, void* dest, const void* src);
typedef void(*FP_Type_drop)(const void* self, void* value);

/* Runtime representation of type with kind `Type`.
*
* Formally: `|- a : Type`.
*/
typedef struct {
  size_t size;
  size_t alignment;
  FP_Type_copy copy;
  FP_Type_move move;
  FP_Type_drop drop;
} Type;

// Get the type's size.
size_t Type_size(const Type* ty);

// Get the type's alignment.
size_t Type_alignment(const Type* ty);

// Copy data from `src` to `dest`.
void Type_copy(const Type* self, void* dest, const void* src);
 
/* Move data from `src` to `dest`.
*
* Ownership of the data is transferred from `src` to `dest`.
*/
void Type_move(const Type* self, void* dest, const void* src);

// Release resources owned by `value`.
void Type_drop(const Type* self, void* value);

/* A `Type` with a single dependency.
*
* Formally: `a : k |- b : Type`.
*/
typedef size_t (*FP_Type1_size)(const void* a);
typedef size_t (*FP_Type1_alignment)(const void* a);
typedef void (*FP_Type1_copy)(const void* self, const void* a, void* dest, const void* src);
typedef void (*FP_Type1_move)(const void* self, const void* a, void* dest, const void* src);
typedef void (*FP_Type1_drop)(const void* self, const void* a, void* value);
typedef struct {
  FP_Type1_size size;
  FP_Type1_alignment alignment;
  FP_Type1_copy copy;
  FP_Type1_move move;
  FP_Type1_drop drop;
} Type1;

size_t Type1_size(const Type1* self, const void* a);
size_t Type1_alignment(const Type1* self, const void* a);
void Type1_copy(const Type1* self, const void* a, void* dest, const void* src);
void Type1_move(const Type1* self, const void* a, void* dest, const void* src);
void Type1_drop(const Type1* self, const void* a, void* value);

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

extern const Type Type_unit;
extern const Type Type_bool;
extern const Type Type_i32;
extern const Type Type_fn;

#endif // type_h
