#include "runtime/type.h"

// i32 : Type
extern const Type Type_i32;

// i64 : Type
extern const Type Type_i64;

// bool : Type
extern const Type Type_bool;

// (,) : Type -> Type -> Type
void* T2_fst(const Type* a, const Type* b, void* t2);
void* T2_snd(const Type* a, const Type* b, void* t2);

// a : Type, b : Type |- a -> b : Type
extern const Type Type_arrow;

typedef struct {
  size_t size;
  size_t alignment;
  FP_Type_copy copy;
  FP_Type_move move;

  // Internal
  const Type* a;
  const Type1* b;
} Type_exists;

void Type_exists_make(Type_exists* result, const Type* a, const Type1* b);

/*
a : k    a : k |- b : Type
--------------------------
   exists a. b : Type
*/
typedef struct {
  // fst : Type
  const Type* fst;

  // snd : b[a:=fst]
  Rc snd;
} Exists;

Exists Exists_alloc(const Type* a, const Type1* b);
