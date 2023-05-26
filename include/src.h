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
  using FilePathStaticString =
      basic::StaticString<meta::Str("src_manager_abs_file_path")>;
  using FilePathStringRef =
      basic::StringRef<meta::Str("src_manager_abs_file_path")>;

  static Manager& instance() {
    static Manager mng;
    return mng;
  }
  uint32_t append(const char* path) {
    auto file = lps::basic::File::create(path);
    if (file == nullptr) {
      return -1;
    }
    auto file_id = file->file_id();
    files_[file_id] = std::move(file);

    return file_id;
  }

  [[nodiscard]] bool has(uint32_t file_id) const {
    return files_.contains(file_id);
  }

  [[nodiscard]] FilePathStringRef path(uint32_t file_id) {
    if (!has(file_id)) {
      LPS_ERROR(meta::Str("manager.path"), "file_id = ", file_id, "not exists");
      return FilePathStringRef();
    }
    if (!abs_file_paths_.contains(file_id)) {
      auto the_path = std::filesystem::canonical(
                          std::filesystem::absolute(files_.at(file_id)->path()))
                          .string();
      abs_file_paths_[file_id] = FilePathStaticString::from(the_path);
    }
    if (!abs_file_paths_.contains(file_id)) {
      LPS_ERROR(meta::Str("manager.abs_file_paths"), "file_id = ", file_id,
                "not exists");
      return FilePathStringRef();
    }

    return FilePathStringRef(abs_file_paths_[file_id]);
  }

  template <meta::Str TagNameOther>
  basic::StringRef<TagNameOther> ref(uint32_t file_id) const {
    if (!has(file_id)) {
      LPS_ERROR(TagNameOther, "file_id = ", file_id, "not exists");
      return basic::StringRef<TagNameOther>();
    }
    return files_.at(file_id)->ref<TagNameOther>();
  }

  [[nodiscard]] size_t size(uint32_t file_id) const {
    if (!has(file_id)) {
      LPS_ERROR(meta::Str("manager.size"), "file_id = ", file_id, "not exists");
      return -1;
    }
    return files_.at(file_id)->size();
  }

 private:
  explicit Manager() = default;
  std::map<uint32_t, basic::File::ptr_type> files_;
  std::map<uint32_t, FilePathStaticString> abs_file_paths_;
};

}  // namespace lps::src
