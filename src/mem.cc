/*
* MIT License
* Copyright (c) 2023 mxlol233 (mxlol233@outlook.com)

* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:

* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.

* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
*/

#include "mem.h"
#include <algorithm>
#include <cstring>
#include <memory>
#include "excption.h"
#include "meta.h"

namespace lps::basic::mem {

namespace details {

template <meta::Str TagName, typename Size, typename T>
Size DynamicBuffer<TagName, Size, T>::new_capacity(Size min,
                                                   Size old_capacity) {
  constexpr auto kTheMaxSize = std::numeric_limits<Size>::max();
  if (min > kTheMaxSize) {
    LPS_ERROR(TagName, "overflow");
  }
  if (old_capacity == kTheMaxSize) {
    LPS_ERROR(TagName, "already at maximum size");
  }
  Size new_capacity = 2 * old_capacity + 1;
  return std::clamp(new_capacity, min, kTheMaxSize);
}

template <meta::Str TagName, typename Size, typename T>
typename DynamicBuffer<TagName, Size, T>::pointer
DynamicBuffer<TagName, Size, T>::malloc_grow(pointer first, Size min,
                                             Size& new_capacity) {

  new_capacity = DynamicBuffer<TagName, Size, T>::new_capacity(min, capacity());

  pointer new_first = allocator_.allocate(new_capacity);
  if (new_first == first) {
    new_first = replace(new_first, new_capacity, new_capacity);
  }
  return new_first;
}

template <meta::Str TagName, typename Size, typename T>
void DynamicBuffer<TagName, Size, T>::grow_pod(pointer first, Size min) {
  auto new_capacity =
      DynamicBuffer<TagName, Size, T>::new_capacity(min, capacity());
  pointer new_first;

  new_first = allocator_.allocate(new_capacity);
  if (new_first == first) {
    new_first = replace(new_first, new_capacity, new_capacity);
  }
  std::copy_n(first_, size(), new_first);

  this->first_ = new_first;
  this->capacity_ = new_capacity;
}

template <meta::Str TagName, typename Size, typename T>
typename DynamicBuffer<TagName, Size, T>::pointer
DynamicBuffer<TagName, Size, T>::replace(pointer first, Size old_capacity,
                                         Size new_capacity) {
  pointer new_first = allocator_.allocate(new_capacity);
  allocator_.deallocate(first, old_capacity);
  return new_first;
}

}  // namespace details

template <meta::Str TagName, size_t BufferSize, typename BlockSizeType,
          typename T>
void MemoryBuffer<TagName, BufferSize, BlockSizeType, T>::expand() {

  LPS_WARNING(
      TagName,
      "buffer index is not valid, maybe the buffer is overflow.\n This will "
      "lead `MemoryBuffer` to use the heap memory [",
      this->tag(), "]");
}

template <meta::Str TagName, size_t BufferSize, typename BlockSizeType,
          typename T>
typename MemoryBuffer<TagName, BufferSize, BlockSizeType, T>::block_ptr_type
MemoryBuffer<TagName, BufferSize, BlockSizeType, T>::block(
    BlockSizeType block_size) {
  block_ptr_type block;
  if (location_ == Location::kStack) {

    if (!this->ok(block_size)) {
      // static buffer is full, now we need to use the heap
      LPS_NOTE(TagName, "static buffer is full");

      location_ = Location::kHeap;
    } else {
      block = block_ptr_type::create(*this, block_size);
    }

  } else {
  }
}

void* malloc(size_t sz) {
  void* result = std::malloc(sz);
  if (result == nullptr) {
    if (sz == 0)
      return lps::basic::mem::malloc(1);
    LPS_ERROR(meta::Str("malloc"), "alloc failed");
  }
  return result;
}

void* realloc(void* ptr, size_t sz) {
  void* result = std::realloc(ptr, sz);
  if (result == nullptr) {
    if (sz == 0)
      return lps::basic::mem::malloc(1);
    LPS_ERROR(meta::Str("realloc"), "alloc failed");
  }
  return result;
}

}  // namespace lps::basic::mem
