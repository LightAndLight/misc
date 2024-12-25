#ifndef rc_h
#define rc_h

#include <stddef.h>

typedef struct Object_t Object;

// A reference-counted pointer.
typedef struct { Object* value; } Rc;

Rc Rc_alloc(size_t alignment, size_t size);

// Increment the reference count.
Rc Rc_copy(Rc ptr);

/* Decrement the reference count.
*
* If the reference count becomes zero, then the underlying memory is freed.
*/
void Rc_free(Rc ptr);

/* Access the underlying memory.
*
* Safety:
*
* The pointer returned by `Rc_data` must not be copied, otherwise the reference count could
* be invalidated. Valid uses of `Rc_data` are:
*
* * Assignment
*
*   * `*(Value*)Rc_data(ptr) = ...;`
*   * `((Struct*)Rc_data(ptr))->field = ...;`
*
* * Access
*
*   * `*(Value*)Rc_data(ptr)`
*   * `((Struct*)Rc_data(ptr))->field`
*/
void* Rc_data(Rc ptr);

#endif // rc_h
