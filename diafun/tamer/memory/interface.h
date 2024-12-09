#pragma once

#include <stddef.h>
#include <stdint.h>

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
#include <stdio.h>

static void take_snapshot(const char* state) {}

static void register_variable(const char* name, const char* type, const void* address, const char* segement) {
    printf("%s @%p [%s]\n", name, address, segement);
}

static void register_array(const char* name, const char* type, const void* address, const char* segement, size_t N) {
    register_variable(name, type, address, segement);
}
#endif
