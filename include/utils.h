#pragma once

#include <cstdint>

// NOTE: log2(0) is undefined
[[nodiscard]] uint64_t log2(uint64_t i);

// Used for static assertions
template <typename...>
inline constexpr bool false_v = false;
