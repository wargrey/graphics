#include <stdio.h>
#include <stdint.h>
#include <math.h>

/*************************************************************************************************/
#define define(type, name) type name; watch_variable(#name, #type, &name, "stack")
#define define_init(type, name, datum) type name = datum; watch_variable(#name, #type, &name, "stack")
#define define_const(type, name, datum) const type name = datum; watch_variable(#name, #type, &name, "stack")
#define define_bss(type, name) static type name; watch_variable(#name, #type, &name, "bss")
#define define_static(type, name, datum) static type name = datum; watch_variable(#name, #type, &name, "data")

#define watch_proc(name) watch_variable(#name, "uintptr_t", &name, "text")
#define watch_text(type, name) watch_variable(#name, #type, &name, "rodata")
#define watch_data(type, name) watch_variable(#name, #type, &name, "data")
#define watch_bss(type, name) watch_variable(#name, #type, &name, "bss")
#define watch_stack(type, name) watch_variable(#name, #type, &name, "stack")

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
static char gdsv = 12;
__ffi__ char gdv = 24;

// .bss
static char gbsv;
__ffi__ char gbv;

// .text (including global constants and function names)
static void swap(char* pa, char* pb) {
    define_bss(char, _t);
    watch_stack(uintptr_t, pa);
    watch_stack(uintptr_t, pb);

    take_snapshot("static swap");

    _t = *pa;
    take_snapshot("a -> t");
    *pa = *pb;
    take_snapshot("b -> a");
    *pb = _t;
    take_snapshot("t -> b (done)");
}

static const char gscv = 20;
__ffi__ const char gcv = 10;
__ffi__ const char gc0; // give up setting the initial value
__ffi__ void _bar() {}

/*************************************************************************************************/
__ffi__ int main() {
    define_bss(char, fbsv);
    define_const(char, fcv, 30);
    define(char, a);
    define(char, b);

    watch_text(char, gscv);
    watch_text(char, gcv);
    watch_text(char, gc0);
    watch_proc(_bar);
    watch_proc(swap);
    watch_proc(take_null);
    watch_proc(main);

    watch_data(char, gdv);
    watch_data(char, gdsv);
    watch_data(uintptr_t, watch_variable);
    watch_data(uintptr_t, take_snapshot);

    watch_bss(char, gbsv);
    watch_bss(char, gbv);

    take_snapshot("defined");

    a = 32;
    b = 64;
    take_snapshot("initialized");

    swap(&a, &b);

    return 0;
}
