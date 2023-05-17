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

#pragma once

#include <filesystem>
#include <map>
#include "basic/exception.h"
#include "basic/file.h"
#include "basic/vec.h"

namespace lps::src {

class Manager {

 public:
  uint32_t append(const char* path) {
    auto file = lps::basic::File::create(path);
    if (file == nullptr) {
      return -1;
    }
    auto file_id = file->file_id();
    std::filesystem::path the_path(path);
    std::string abs_path = std::filesystem::absolute(the_path).string();
    if (!files_.contains(abs_path)) {
      if (file_ids_.contains(file_id)) {
        unreachable(meta::S("src_manager"));
      }
      files_[abs_path] = std::move(file);
      file_ids_[file_id] = abs_path;
    }
    return file_id;
  }

  [[nodiscard]] bool has(uint32_t file_id) const {
    auto ok0 = file_ids_.contains(file_id);
    if (ok0) {
      if (files_.contains(file_ids_.at(file_id))) {
        return true;
      }
    }
    return false;
  }

  template <meta::Str TagNameOther>
  basic::StringRef<TagNameOther> ref(uint32_t file_id) {
    if (!has(file_id)) {
      return basic::StringRef<TagNameOther>();
    }
    return files_.at(file_ids_.at(file_id))->ref<TagNameOther>();
  }

 private:
  std::map<std::string, basic::File::ptr_type> files_;
  std::map<uint32_t, std::string> file_ids_;
};

}  // namespace lps::src
