#ifndef closure_h
#define closure_h

typedef void (*FP_Closure_apply)(void* self, ...);

typedef struct {
  /* Closures have different calling conventions when they're monomorphic or polymorphic.
  *
  * * Monomorphic return type: `X (*apply)(void* self, ...);`
  * * Polymorphic return type: `void (*apply)(void* result, void* self, ...);`
  * * Monomorphic argument: `void (*apply)(void* self, X arg);`
  * * Polymorpic argument: `void (*apply)(void* self, void* arg);`
  */
  FP_Closure_apply apply;
} Closure;

#define FP_Closure_applyPP() void (*)(void* result, const Closure* self, void* arg)
#define FP_Closure_applyPM(output) output (*)(const Closure* self, void* arg)
#define FP_Closure_applyMM(input, output) output (*)(const Closure* self, input arg)

#define Closure_applyPP(result, f, arg) ((FP_Closure_applyPP())((f)->apply))(result, f, arg)
#define Closure_applyPM(b, f, arg) ((FP_Closure_applyPM(b))((f)->apply))(f, arg)
#define Closure_applyMM(a, b, f, arg) ((FP_Closure_applyMM(a, b))((f)->apply))(f, arg)

#endif // closure_h
