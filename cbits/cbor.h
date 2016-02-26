/* Needed GHC definitions */
#include "MachDeps.h"

/*
** GHC 7.10 and above include efficient byte-swapping primitives,
** which are useful for efficient byte-mangling routines.
*/
#if __GLASGOW_HASKELL__ >= 710
#define HAVE_BYTESWAP_PRIMOPS
#endif

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
