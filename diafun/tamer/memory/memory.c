#include "interface.h"

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
