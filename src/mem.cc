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
#include <cstring>
#include "excption.h"

namespace lps::basic::mem {

namespace details {

template <meta::Str TagName, typename Size>
Size Heap<TagName, Size>::new_capacity(Size min, Size old_capacity) {
  constexpr auto kTheMaxSize = std::numeric_limits<Size>::max();
  if (min > kTheMaxSize) {
    LPS_ERROR("overflow");
  }
  if (old_capacity == kTheMaxSize) {
    LPS_ERROR("already at maximum size");
  }
  Size new_capacity = 2 * old_capacity + 1;
  return std::clamp(new_capacity, min, kTheMaxSize);
}

template <meta::Str TagName, typename Size>
void* Heap<TagName, Size>::malloc_grow(void* first, Size min, Size type_size,
                                       Size& new_capacity) {

  new_capacity = Heap<TagName, Size>::new_capacity(min, capacity());

  void* new_first = mem::malloc(new_capacity * type_size);
  if (new_first == first) {
    new_first = replace(new_first, type_size, new_capacity);
  }
  return new_first;
}

template <meta::Str TagName, typename Size>
void Heap<TagName, Size>::grow_pod(void* first, Size min, Size type_size) {
  auto new_capacity = Heap<TagName, Size>::new_capacity(min, capacity());
  void* new_first;
  if (first == first_) {
    new_first = mem::malloc(type_size * new_capacity);
    if (new_first == first) {
      new_first = replace(new_first, type_size, new_capacity);
    }
    std::memcpy(new_first, first_, type_size * size());
  } else {
    new_first = mem::realloc(first_, new_capacity * type_size);
    if (new_first == first)
      new_first = replace(new_first, type_size, new_capacity, size());
  }
  this->first_ = new_first;
  this->capacity_ = new_capacity;
}

template <meta::Str TagName, typename Size>
void* Heap<TagName, Size>::replace(void* first, Size type_size,
                                   Size new_capacity, Size vsize) {
  void* new_first = mem::malloc(new_capacity * type_size);
  if (vsize)
    std::memcpy(new_first, first, vsize * type_size);
  free(first);
  return new_first;
}

}  // namespace details

template <meta::Str TagName, size_t NBuffer, size_t BufferSize,
          typename BlockSizeType, typename T>
void MemoryBuffer<TagName, NBuffer, BufferSize, BlockSizeType, T>::expand() {
  bool cond = next_buffer_pos_ >= 0 && next_buffer_pos_ < NBuffer;
  if (cond) {
    buffer_pos_ = next_buffer_pos_;
    buffers_[next_buffer_pos_++] =
        MemoryBuffer<TagName, NBuffer, BufferSize, BlockSizeType,
                     T>::buffer_type::create();
  } else {

    LPS_WARNING(
        "buffer index is not valid, maybe the buffer is overflow.\n This will "
        "lead `MemoryBuffer` to use the heap memory [",
        this->tag(), "]");
  }
}

void* malloc(size_t sz) {
  void* result = std::malloc(sz);
  if (result == nullptr) {
    if (sz == 0)
      return lps::basic::mem::malloc(1);
    LPS_ERROR("alloc failed");
  }
  return result;
}

void* realloc(void* ptr, size_t sz) {
  void* result = std::realloc(ptr, sz);
  if (result == nullptr) {
    if (sz == 0)
      return lps::basic::mem::malloc(1);
    LPS_ERROR("alloc failed");
  }
  return result;
}

}  // namespace lps::basic::mem
