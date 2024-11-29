#include <stdio.h>
#include <stdint.h>
#include <math.h>

/*************************************************************************************************/
#define define(type, name) type name; watch_variable(#name, #type, &name, "stack")
#define define_init(type, name, datum) type name = datum; watch_variable(#name, #type, &name, "stack")
#define define_const(type, name, datum) const type name = datum; watch_variable(#name, #type, &name, "stack")
#define define_static(type, name, datum) static type name = datum; watch_variable(#name, #type, &name, ".data")

#define watch_proc(name) watch_variable(#name, "uintptr_t", &name, ".text")
#define watch_text(type, name) watch_variable(#name, #type, &name, ".text")
#define watch_data(type, name) watch_variable(#name, #type, &name, ".data")
#define watch_bss(type, name) watch_variable(#name, #type, &name, ".bss")

static void take_null(const char* state) {}

#ifdef __racket__
typedef void (*watch_variable_t)(const char* name, const char* type, const void* address, const char* segment);
typedef void (*take_snapshot_t)(const char* reason);

static void watch_nothing(const char* name, const char* type, const void* address, const char* segement) {}

__ffi__ watch_variable_t watch_variable = watch_nothing;
__ffi__ take_snapshot_t  take_snapshot = take_null;

#else
#define take_snapshot take_null

static void watch_variable(const char* name, const char* type, const void* address, const char* segement) {
    printf("%s @%p [%s]\n", name, address, segement);
}
#endif

// .data
static char gsv = 12;
__ffi__ char gv = 24;

// .bss
static char bsv;
__ffi__ char bv;

// .text (including global constants and function names)
static const char gscv = 20;
__ffi__ const char gcv = 10;
__ffi__ const char gc0; // give up setting the initial value
__ffi__ static void _foo() {}
__ffi__ void bar() {}

/*************************************************************************************************/
__ffi__ int main() {
    define_static(char, fsv, 10);
    define_const(char, fcv, 30);
    define(char, a);
    define(char, b);
    define(char, _t);

    watch_text(char, gscv);
    watch_text(char, gcv);
    watch_text(char, gc0);
    watch_proc(bar);
    watch_proc(_foo);
    watch_proc(take_null);
    watch_proc(main);

    watch_data(char, gv);
    watch_data(char, gsv);
    watch_data(uintptr_t, watch_variable);
    watch_data(uintptr_t, take_snapshot);

    watch_bss(char, bsv);
    watch_bss(char, bv);

    take_snapshot("defined");

    a = 32;
    b = 64;
    take_snapshot("initialized");
    
    _t = a;
    take_snapshot("a -> t");
    a = b;
    take_snapshot("b -> a");
    b = _t;
    take_snapshot("t -> b");

    return 0;
}
