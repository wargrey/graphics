#include <stdio.h>
#include <stdint.h>
#include <math.h>

#ifndef __ffi__
#define __ffi__
#endif

/*************************************************************************************************/
#define define(type, name) type name; register_variable(#name, #type, &name, "stack")
#define define_init(type, name, datum) type name = datum; register_variable(#name, #type, &name, "stack")
#define define_const(type, name, datum) const type name = datum; register_variable(#name, #type, &name, "stack")
#define define_bss(type, name) static type name; register_variable(#name, #type, &name, "bss")
#define define_static(type, name, datum) static type name = datum; register_variable(#name, #type, &name, "data")

#define watch_proc(name) register_variable(#name, "uintptr_t", &name, "text")
#define watch_text(type, name) register_variable(#name, #type, &name, "rodata")
#define watch_data(type, name) register_variable(#name, #type, &name, "data")
#define watch_bss(type, name) register_variable(#name, #type, &name, "bss")
#define watch_stack(type, name) register_variable(#name, #type, &name, "stack")

/*************************************************************************************************/
#define define_array(type, name, N) type name[N]; register_array(#name, #type, name, "stack", N)

#define watch_array(type, name) register_array(#name, #type, name, "stack", sizeof(name) / sizeof(type))
#define watch_vector(type, name) register_array(#name, #type, name, "data", sizeof(name) / sizeof(type))
#define watch_tuple(type, name) register_array(#name, #type, name, "rodata", sizeof(name) / sizeof(type))

/*************************************************************************************************/
#ifdef __racket__
typedef void (*take_snapshot_t)(const char* reason);
typedef void (*register_variable_t)(const char* name, const char* type, const void* address, const char* segment);
typedef void (*register_array_t)(const char* name, const char* type, const void* address, const char* segment, size_t N);

// .bss
__ffi__ take_snapshot_t take_snapshot;
__ffi__ register_variable_t register_variable;
__ffi__ register_array_t register_array;

#else
static void take_snapshot(const char* state) {}

static void register_variable(const char* name, const char* type, const void* address, const char* segement) {
    printf("%s @%p [%s]\n", name, address, segement);
}

static void register_array(const char* name, const char* type, const void* address, const char* segement, size_t N) {
    watch_variable(name, type, address, segement);
}
#endif

// .data
static char data_gsv = 0x12;
__ffi__ char data_gv = 0x24;
__ffi__ char data_gchrs[] = "chars";

// .bss
static char unset_gsv;
__ffi__ char unset_gv;

// .text (including global constants and function names)
static void swap(uint8_t* ab, size_t n) {
    define_bss(uint8_t, t);
    watch_stack(uintptr_t, ab);
    watch_stack(size_t, n);

    take_snapshot("swap's frame");

    t = ab[0];
    take_snapshot("a -> t");
    ab[0] = ab[1];
    take_snapshot("b -> a");
    ab[1] = t;
    take_snapshot("t -> b (done)");
}

static const char const_gsv = 0x10;
__ffi__ const char const_gv = 0x20;
__ffi__ const char const_g0; // give up setting the initial value
__ffi__ const char const_gchrs[] = "const\nchars";
__ffi__ const char* const const_gstr = "string";

/*************************************************************************************************/
__ffi__ int main(int argc, char* argv[]) {
    const uint8_t src [] = { 0x2, 0x4 };
    define_bss(uint8_t, unset_fsv);
    define_const(uint8_t, const_fv, 0x30);
    define_array(uint8_t, ab, src[0]);
    
    watch_text(uintptr_t, const_gstr);
    watch_text(uint8_t, const_gsv);
    watch_text(uint8_t, const_gv);
    watch_text(uint8_t, const_g0);
    watch_proc(swap);
    watch_proc(main);

    watch_data(uint8_t, data_gv);
    watch_data(uint8_t, data_gsv);
    watch_bss(uintptr_t, take_snapshot);
    watch_bss(uintptr_t, register_variable);
    watch_bss(uintptr_t, register_array);

    watch_bss(uint8_t, unset_gsv);
    watch_bss(uint8_t, unset_gv);

    watch_tuple(char, const_gchrs);
    watch_vector(char, data_gchrs);
    watch_array(uint8_t, src);
    
    take_snapshot("defined");

    ab[0] = src[0];
    ab[1] = src[1];
    take_snapshot("initialized");

    swap(ab, sizeof(ab) / sizeof(uint8_t));

    return 0;
}
