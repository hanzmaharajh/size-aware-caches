#include "utils.h"

#include <climits>
#include <cstdint>

#if defined(_MSC_VER)
#include <immintrin.h>
#endif

// https://godbolt.org/z/ggSGRZ
uint64_t log2(uint64_t i) {
#if defined(_MSC_VER) && defined(_M_X64)
  return sizeof(i) * CHAR_BIT - 1 - _lzcnt_u64(i);
#elif (defined(__GNUC__) || defined(__clang__)) && \
    (defined(__x86_64__) || defined(__aarch64__))
  return sizeof(i) * CHAR_BIT - 1 - __builtin_clzll(i);
#else
  uint64_t retval = 0;
  while (i >>= 1) ++retval;
  return retval;
#endif
}
