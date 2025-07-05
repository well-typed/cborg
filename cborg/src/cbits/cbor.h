/* Needed GHC definitions */
#include "MachDeps.h"

/*
** On Intel 32/64 bit machines, memory access to unaligned addresses
** is permitted (and generally efficient, too). With this in mind,
** some operations can be implemented more efficiently.
*/
#if i386_HOST_ARCH || x86_64_HOST_ARCH
#define MEM_UNALIGNED_OPS
#endif

/*
** Establish the word-size of the machine, or fail.
*/
#if WORD_SIZE_IN_BITS == 64
#define ARCH_64bit
#elif WORD_SIZE_IN_BITS == 32
#define ARCH_32bit
#else
#error expected WORD_SIZE_IN_BITS to be 32 or 64
#endif
