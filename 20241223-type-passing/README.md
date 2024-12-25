#  Type-passing compiler

*2024-12-23*

Development has moved to <https://github.com/LightAndLight/type-passing-compilation>.

A proof-of-concept for compiling parametric polymorphism without monomorphisation or boxing-by-default.
Datatypes are unboxed/stack-allocated by default, an are passed to polmorphic functions by
reference. Type variables (e.g. the `a` in `forall a. a -> a`) become extra runtime arguments
consisting of a vtable that knows how to manipulate pointers to values of the underlying type.

## Example

### Input

```haskell
id :: a -> a
id x = x

something :: Int32
something = id 99
```

### Output

```c
typedef struct Type {
  size_t size;
  size_t aligment;
  void(move*)(const Type* self, void* dest, const void* src);
  /* other members */;
}

void Type_Int32_move(const Type* self, int32_t* dest, const int32_t* src) {
  *dest = *src;
}

const Type_Int32 = {
  .size = 4,
  .alignment = 4,
  .move = &Type_Int32_move,
  /* other members */
}

void id(void* result, const Type* x, const void* a) {
  x->move(x, result, a);
}

int32_t something() {
  int32_t arg = 99;
  int32_t result; id(&Type_Int32, &result, &arg);
  return result
}
```

