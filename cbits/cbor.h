#include "MachDeps.h"

#if __GLASGOW_HASKELL__ >= 710
#define HAVE_BYTESWAP_PRIMOPS
#endif

#if i386_HOST_ARCH || x86_64_HOST_ARCH
#define MEM_UNALIGNED_OPS
#endif

#if WORD_SIZE_IN_BITS == 64
#define ARCH_64bit
#elif WORD_SIZE_IN_BITS == 32
#define ARCH_32bit
#else
#error expected WORD_SIZE_IN_BITS to be 32 or 64
#endif
