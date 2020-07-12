#include "sizeawarecaches/size_aware_cache.h"
#include "sizeawarecaches/utils.h"

#include <chrono>
#include <cmath>
#include <numeric>

#include <gtest/gtest.h>

using namespace std::chrono_literals;

// A clock struct to test with
struct TestClock {
  using time_point = std::chrono::steady_clock::time_point;
  using duration = std::chrono::steady_clock::duration;

  static time_point current_time;

  [[nodiscard]] static time_point now() { return current_time; }
};
TestClock::time_point TestClock::current_time{};

// Helper class to calculate cache entry sizes.
//
// NOTE: This helper is used for all test fixtures and cases below.
struct SizeCalculator {
  [[nodiscard]] size_t operator()(const std::shared_ptr<size_t>& s) {
    return *s;
  }

  [[nodiscard]] size_t operator()(const std::vector<size_t>& s) {
    return std::size(s);
  }

  [[nodiscard]] size_t operator()(const size_t& s) { return s; }
};

using all_test_types = ::testing::Types<
    std::integral_constant<CachingStrategy, CachingStrategy::LRU>,
    std::integral_constant<CachingStrategy, CachingStrategy::SizeAwareLRU>,
    std::integral_constant<CachingStrategy,
                           CachingStrategy::SizeAndPopularityAwareLRU>>;
// Templated fixture
template <typename T>
class Cache_size10Test
    : public LRUCache<size_t, std::shared_ptr<size_t>, T::value, SizeCalculator,
                      std::less<size_t>, TestClock>,
      public ::testing::Test {
 protected:
  Cache_size10Test()
      : LRUCache<size_t, std::shared_ptr<size_t>, T::value, SizeCalculator,
                 std::less<size_t>, TestClock>(10, 5) {
    TestClock::current_time = {};
  }
};

TYPED_TEST_CASE(Cache_size10Test, all_test_types);

TYPED_TEST(Cache_size10Test, Init) { EXPECT_EQ(this->cache_size(), 0); }

TYPED_TEST(Cache_size10Test, MoveConsruct) {
  LRUCache<size_t, std::shared_ptr<size_t>, TypeParam::value, SizeCalculator,
           std::less<size_t>, TestClock>
      cache1(5, 10);

  auto ptr1 = std::make_shared<size_t>(1);

  {
    const auto& [inserted_value, inserted] = cache1.insert({1, ptr1});
    EXPECT_TRUE(inserted);
  }

  LRUCache cache2(std::move(cache1));
  EXPECT_EQ(ptr1.use_count(), 2);

  EXPECT_EQ(cache2.cache_size(), 1);
  EXPECT_EQ(cache2.fetch(1)->get(), ptr1.get());
}

TYPED_TEST(Cache_size10Test, MoveAssign) {
  LRUCache<size_t, std::shared_ptr<size_t>, TypeParam::value, SizeCalculator,
           std::less<size_t>, TestClock>
      cache1(5, 10);

  auto ptr1 = std::make_shared<size_t>(1);

  {
    const auto& [inserted_value, inserted] = cache1.insert({1, ptr1});
    EXPECT_TRUE(inserted);
  }

  LRUCache<size_t, std::shared_ptr<size_t>, TypeParam::value, SizeCalculator,
           std::less<size_t>, TestClock>
      cache2(5, 10);
  cache2 = std::move(cache1);
  EXPECT_EQ(ptr1.use_count(), 2);

  EXPECT_EQ(cache2.cache_size(), 1);
  EXPECT_EQ(cache2.fetch(1)->get(), ptr1.get());
}

TYPED_TEST(Cache_size10Test, FetchEraseNonExistent) {
  EXPECT_EQ(this->fetch(1), nullptr);
  EXPECT_EQ(this->erase(1), 0);
}

TYPED_TEST(Cache_size10Test, FetchEraseExisting) {
  auto ptr1 = std::make_shared<size_t>(1);

  {
    const auto& [inserted_value, inserted] = this->insert({1, ptr1});
    EXPECT_TRUE(inserted);
  }

  EXPECT_EQ(this->fetch(1)->get(), ptr1.get());

  EXPECT_EQ(this->erase(1), 1);
  EXPECT_FALSE(this->fetch(1));
}

TYPED_TEST(Cache_size10Test, InsertTwiceFails) {
  auto ptr4 = std::make_shared<size_t>(4);

  {
    const auto& [inserted_value, inserted] = this->insert({1, ptr4});
    EXPECT_TRUE(inserted);
    EXPECT_EQ(ptr4, inserted_value);
  }

  auto ptr8 = std::make_shared<size_t>(8);

  {
    const auto& [inserted_value, inserted] = this->insert({1, ptr8});
    EXPECT_FALSE(inserted);
    EXPECT_NE(ptr8, inserted_value);
  }
}

TYPED_TEST(Cache_size10Test, Overflow_HalfRemains) {
  std::vector<std::shared_ptr<size_t>> elements;

  for (size_t i = 0; i < 10; ++i) {
    this->insert({i, elements.emplace_back(std::make_shared<size_t>(1))});
  }
  EXPECT_EQ(this->cache_size(), 10);

  auto ptr = std::make_shared<size_t>(1);
  this->insert({10, ptr});

  EXPECT_EQ(this->cache_size(), 5);
  EXPECT_EQ(ptr.use_count(), 2);

  const auto& others_remaining_in_cache = std::accumulate(
      std::begin(elements), std::end(elements), 0u,
      [](size_t acc, const auto& promise) {
        return acc + static_cast<size_t>(promise.use_count() > 1);
      });

  EXPECT_EQ(others_remaining_in_cache, 4);
}

TYPED_TEST(Cache_size10Test, Overflow_OneRemains) {
  std::vector<std::shared_ptr<size_t>> elements;

  for (size_t i = 0; i < 10; ++i) {
    this->insert({i, elements.emplace_back(std::make_shared<size_t>(1))});
  }
  EXPECT_EQ(this->cache_size(), 10);

  auto ptr = std::make_shared<size_t>(5);
  this->insert({10, ptr});

  EXPECT_EQ(this->cache_size(), 5);
  EXPECT_EQ(ptr.use_count(), 2);

  const auto& others_remaining_in_cache = std::accumulate(
      std::begin(elements), std::end(elements), 0u,
      [](size_t acc, const auto& promise) {
        return acc + static_cast<size_t>(promise.use_count() > 1);
      });

  EXPECT_EQ(others_remaining_in_cache, 0);
}

TYPED_TEST(Cache_size10Test, Evict_LRU) {
  std::vector<std::shared_ptr<size_t>> elements;

  for (size_t i = 0; i < 10; ++i) {
    this->insert({i, elements.emplace_back(std::make_shared<size_t>(1))});
  }

  // Cache is full
  EXPECT_EQ(this->cache_size(), 10);

  // Touch existing element
  EXPECT_TRUE(this->fetch(2));

  this->pro_evict(5);

  EXPECT_EQ(this->cache_size(), 5);

  std::vector<size_t> use_counts;
  std::transform(std::begin(elements), std::end(elements),
                 std::back_inserter(use_counts),
                 [](const auto& promise) { return promise.use_count(); });

  std::vector<size_t> counts({1, 1, 2, 1, 1, 1, 2, 2, 2, 2});
  EXPECT_EQ(use_counts, counts);

  for (size_t i = 0; i < std::size(counts); ++i) {
    EXPECT_EQ(static_cast<bool>(this->fetch(i)), counts[i] > 1);
  }

  EXPECT_EQ(this->size(), 5);
}

TYPED_TEST(Cache_size10Test, Evict_SmallSize) {
  using namespace std::chrono_literals;

  // Small elements inserted at time = 0
  std::vector<std::shared_ptr<size_t>> small_elements;
  for (size_t i = 0; i < 6; ++i) {
    this->insert({i, small_elements.emplace_back(std::make_shared<size_t>(1))});
  }

  TestClock::current_time += 4s;

  auto ptr6 = std::make_shared<size_t>(4);
  this->insert({6, ptr6});

  // Cache is full
  EXPECT_EQ(this->cache_size(), 10);

  TestClock::current_time += 1s;

  this->pro_evict(4);

  // The small elements were there more than four times as long as the large
  // element. So, they should be evicted.
  EXPECT_EQ(this->cache_size(), 4);
  EXPECT_EQ(ptr6.use_count(), 2);
}

using multibucket_test_types = ::testing::Types<
    std::integral_constant<CachingStrategy, CachingStrategy::SizeAwareLRU>,
    std::integral_constant<CachingStrategy,
                           CachingStrategy::SizeAndPopularityAwareLRU>>;

// Templated fixture
template <typename T>
class MultibucketCache_size10Test
    : public LRUCache<size_t, std::shared_ptr<size_t>, T::value, SizeCalculator,
                      std::less<size_t>, TestClock>,
      public ::testing::Test {
 protected:
  MultibucketCache_size10Test()
      : LRUCache<size_t, std::shared_ptr<size_t>, T::value, SizeCalculator,
                 std::less<size_t>, TestClock>(10, 5) {
    TestClock::current_time = {};
  }
};

TYPED_TEST_CASE(MultibucketCache_size10Test, multibucket_test_types);

TYPED_TEST(MultibucketCache_size10Test, InsertNoEvictDifferentBucket) {
  auto ptr1 = std::make_shared<size_t>(1);

  {
    const auto& [inserted_value, inserted] = this->insert({1, ptr1});
    EXPECT_TRUE(inserted);
  }

  EXPECT_EQ(this->cache_size(), 1);
  EXPECT_EQ(ptr1.use_count(), 2);

  auto ptr4 = std::make_shared<size_t>(4);

  {
    const auto& [inserted_value, inserted] = this->insert({2, ptr4});
    EXPECT_TRUE(inserted);
  }

  EXPECT_EQ(this->cache_size(), 5);
  EXPECT_EQ(ptr1.use_count(), 2);
  EXPECT_EQ(ptr4.use_count(), 2);

  EXPECT_EQ(std::size(this->buckets[0]), 1);
  EXPECT_EQ(std::size(this->buckets[2]), 1);
}

TYPED_TEST(MultibucketCache_size10Test, InsertNoEvictSameBucket) {
  auto ptr1 = std::make_shared<size_t>(1);

  {
    const auto& [inserted_value, inserted] = this->insert({1, ptr1});
    EXPECT_TRUE(inserted);
  }

  EXPECT_EQ(this->cache_size(), 1);
  EXPECT_EQ(ptr1.use_count(), 2);

  auto ptr1_prime = std::make_shared<size_t>(1);

  {
    const auto& [inserted_value, inserted] = this->insert({2, ptr1_prime});
    EXPECT_TRUE(inserted);
  }

  EXPECT_EQ(this->cache_size(), 2);
  EXPECT_EQ(ptr1.use_count(), 2);
  EXPECT_EQ(ptr1_prime.use_count(), 2);

  EXPECT_EQ(std::size(this->buckets[0]), 2);
}

TYPED_TEST(MultibucketCache_size10Test, InsertEvictDifferentBucket) {
  auto ptr4 = std::make_shared<size_t>(4);

  {
    const auto& [inserted_value, inserted] = this->insert({1, ptr4});
    EXPECT_TRUE(inserted);
  }

  EXPECT_EQ(this->cache_size(), 4);
  EXPECT_EQ(ptr4.use_count(), 2);
  EXPECT_EQ(std::size(this->buckets[2]), 1);

  auto ptr16 = std::make_shared<size_t>(16);

  {
    const auto& [inserted_value, inserted] = this->insert({2, ptr16});
    EXPECT_TRUE(inserted);
  }

  EXPECT_EQ(this->cache_size(), 16);
  EXPECT_EQ(ptr4.use_count(), 1);
  EXPECT_EQ(ptr16.use_count(), 2);
  EXPECT_EQ(std::size(this->buckets[2]), 0);
  EXPECT_EQ(std::size(this->buckets[4]), 1);
}

TYPED_TEST(MultibucketCache_size10Test, InsertEvictSameBucket) {
  auto ptr8 = std::make_shared<size_t>(8);

  {
    const auto& [inserted_value, inserted] = this->insert({1, ptr8});
    EXPECT_TRUE(inserted);
  }

  EXPECT_EQ(this->cache_size(), 8);
  EXPECT_EQ(ptr8.use_count(), 2);
  EXPECT_EQ(std::size(this->buckets[3]), 1);

  auto ptr8_prime = std::make_shared<size_t>(8);

  {
    const auto& [inserted_value, inserted] = this->insert({2, ptr8_prime});
    EXPECT_TRUE(inserted);
  }

  EXPECT_EQ(this->cache_size(), 8);
  EXPECT_EQ(ptr8.use_count(), 1);
  EXPECT_EQ(ptr8_prime.use_count(), 2);
  EXPECT_EQ(std::size(this->buckets[3]), 1);
}

TYPED_TEST(MultibucketCache_size10Test, Evict_LargeSize) {
  // Small elements inserted at time = 0
  std::vector<std::shared_ptr<size_t>> small_elements;
  for (size_t i = 0; i < 6; ++i) {
    this->insert({i, small_elements.emplace_back(std::make_shared<size_t>(1))});
  }

  TestClock::current_time += 1s;

  auto ptr6 = std::make_shared<size_t>(4);
  this->insert({6, ptr6});

  // Cache is full
  EXPECT_EQ(this->cache_size(), 10);

  TestClock::current_time += 1s;

  this->pro_evict(4);

  // Even though the small elements were there twice as long, the large element
  // should be evicted.
  EXPECT_EQ(this->cache_size(), 4);
  EXPECT_EQ(ptr6.use_count(), 1);
}
TEST(SizeAwareCacheTests, MoveValueWorks) {
  // Just to demo passing the calculator via the constructor, we will count the
  // calls to the calculator in this test.
  struct SizeCalculatorX {
    std::shared_ptr<size_t> calls = std::make_shared<size_t>(0);
    size_t operator()(const std::vector<size_t>& s) {
      ++(*calls);
      return std::size(s);
    }
  } size_calculator;

  LRUCache<size_t, std::vector<size_t>,
           CachingStrategy::SizeAndPopularityAwareLRU, SizeCalculatorX,
           std::less<size_t>, TestClock>
      cache(10, 5, size_calculator);

  {
    std::vector<size_t> v(5);
    const auto& data_ptr = std::data(v);
    const auto& [inserted_value, inserted] =
        cache.insert({0, v});  // insert(pair&&)
    EXPECT_NE(std::data(inserted_value), data_ptr);
    EXPECT_NE(cache.fetch(0)->data(), data_ptr);
    EXPECT_EQ(std::size(v), 5);
  }

  {
    std::vector<size_t> v(5);
    const auto& data_ptr = std::data(v);
    const auto& promise = std::make_pair(1, std::move(v));
    const auto& [inserted_value, inserted] =
        cache.insert(promise);  // insert(const pair&)
    EXPECT_NE(std::data(inserted_value), data_ptr);
    EXPECT_NE(cache.fetch(1)->data(), data_ptr);
    EXPECT_EQ(std::size(promise.second), 5);
  }

  {
    std::vector<size_t> v(5);
    const auto& data_ptr = std::data(v);
    const auto& [inserted_value, inserted] =
        cache.insert({2, std::move(v)});  // insert(pair&&)
    EXPECT_EQ(std::data(inserted_value), data_ptr);
    EXPECT_EQ(cache.fetch(2)->data(), data_ptr);
    EXPECT_EQ(std::size(v), 0);
  }

  EXPECT_EQ(*size_calculator.calls, 3);
}

template <typename T>
class Cache_MoveTest
    : public LRUCache<std::string, size_t, T::value, SizeCalculator,
                      std::less<std::string>, TestClock>,
      public ::testing::Test {
 protected:
  Cache_MoveTest()
      : LRUCache<std::string, size_t, T::value, SizeCalculator,
                 std::less<std::string>, TestClock>(10, 5) {
    TestClock::current_time = {};
  }
};

TYPED_TEST_CASE(Cache_MoveTest, all_test_types);

TYPED_TEST(Cache_MoveTest, MoveKeyWorks) {
  {
    std::string str_a(100, 'A');
    const auto str_a_copy = str_a;
    const auto& data_ptr = std::data(str_a);

    const auto& [inserted_value, inserted] =
        this->insert({std::move(str_a), 1});  // insert(pair&&)

    EXPECT_EQ(std::size(str_a), 0);
    EXPECT_EQ(std::data(this->map.find(str_a_copy)->first), data_ptr);
  }

  {
    std::string str_a(100, 'A');
    const auto str_a_copy = str_a;
    const auto& data_ptr = std::data(str_a);

    const auto& promise = std::make_pair(std::move(str_a), 1);

    const auto& [inserted_value, inserted] =
        this->insert(promise);  // insert(const pair&)

    EXPECT_EQ(std::size(promise.first), 100);
    EXPECT_NE(std::data(this->map.find(str_a_copy)->first), data_ptr);
  }
}

class SizeAndPopularityAwareCache_size10Test
    : public MultibucketCache_size10Test<std::integral_constant<
          CachingStrategy, CachingStrategy::SizeAndPopularityAwareLRU>> {};

TEST_F(SizeAndPopularityAwareCache_size10Test, Evict_Unpopular) {
  auto ptr1 = std::make_shared<size_t>(8);
  this->insert({1, ptr1});

  EXPECT_EQ(std::size(this->buckets[3]), 1);

  // Make ptr1 popular
  this->fetch(1);
  EXPECT_EQ(std::size(this->buckets[2]), 1);
  for (size_t i = 0; i < 2; ++i) {
    this->fetch(1);
  }
  EXPECT_EQ(std::size(this->buckets[2]), 0);
  EXPECT_EQ(std::size(this->buckets[1]), 1);
  for (size_t i = 0; i < 4; ++i) {
    this->fetch(1);
  }
  EXPECT_EQ(std::size(this->buckets[1]), 0);
  EXPECT_EQ(std::size(this->buckets[0]), 1);
  // It's popular!

  auto ptr2 = std::make_shared<size_t>(2);
  this->insert({2, ptr2});

  // We need some time to pass, otherwise all scores are zero.
  TestClock::current_time += 1s;

  // Even though ptr1 is much larger, it is popular. We don't expect it to be
  // selected for eviction.
  this->pro_evict(8);

  EXPECT_EQ(ptr1.use_count(), 2);
  EXPECT_EQ(ptr2.use_count(), 1);
}

TEST(IntegerLog2, Singles) {
  // This test isn't parameterized because parameterization of 1,000,000 tests
  // takes too long
  for (uint64_t i = 1; i < 1'000'000; ++i) {
    EXPECT_EQ(log2(i), static_cast<uint64_t>(std::log2(i)));
  }
}

class IntegerLog2Fixture : public ::testing::TestWithParam<uint64_t> {};

TEST_P(IntegerLog2Fixture, Log2) {
  uint64_t i = GetParam();
  EXPECT_EQ(log2(i), static_cast<uint64_t>(std::log2(i)));
}

const auto powers_of_2 = [] {
  // Only up to 45 because the floating point log2 gets a little dodgy for big
  // numbers
  std::array<uint64_t, 45> v;
  std::generate_n(std::begin(v), std::size(v),
                  [i = 1]() mutable { return uint64_t{1} << i++; });
  return v;
}();

INSTANTIATE_TEST_CASE_P(Powers, IntegerLog2Fixture,
                        ::testing::ValuesIn(std::begin(powers_of_2),
                                            std::end(powers_of_2)));

const auto powers_of_2_plus_1 = [] {
  auto v = powers_of_2;
  std::transform(std::begin(v), std::end(v), std::begin(v),
                 [](const auto& i) { return i + 1; });
  return v;
}();

INSTANTIATE_TEST_CASE_P(PowersPlus1, IntegerLog2Fixture,
                        ::testing::ValuesIn(std::begin(powers_of_2_plus_1),
                                            std::end(powers_of_2_plus_1)));

const auto powers_of_2_minus_1 = [] {
  auto v = powers_of_2;
  std::transform(std::begin(v), std::end(v), std::begin(v),
                 [](const auto& i) { return i - 1; });
  return v;
}();

INSTANTIATE_TEST_CASE_P(PowersMinus1, IntegerLog2Fixture,
                        ::testing::ValuesIn(std::begin(powers_of_2_minus_1),
                                            std::end(powers_of_2_minus_1)));
