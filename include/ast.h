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

#include <array>
#include <utility>
#include "basic/apn.h"
#include "basic/map.h"
#include "basic/vec.h"
#include "token.h"

namespace lps::parser::details {

enum class ParseFunctionKind : uint16_t {
  kUnknown = 0,
  kExpectedToken = 1,
#define PARSE_FUNC(FUNC) FUNC,
#include "parse_function/kinds.def"
  kNum,
};

namespace kind {

static constexpr std::array<std::pair<ParseFunctionKind, const char*>,
                            static_cast<uint16_t>(ParseFunctionKind::kNum)>
    kLists = {{
        {ParseFunctionKind::kUnknown, "kUnknown"},
        {ParseFunctionKind::kExpectedToken, "kExpectedToken"},
#define PARSE_FUNC(X) {ParseFunctionKind::X, #X},
#include "parse_function/kinds.def"
    }};

static constexpr lps::basic::map::Map<ParseFunctionKind, const char*,
                                      static_cast<uint16_t>(
                                          ParseFunctionKind::kNum)>
    kMap{kLists};

}  // namespace kind

inline std::ostream& operator<<(std::ostream& s, ParseFunctionKind kind) {
  s << kind::kMap.at(kind);
  return s;
}

struct Line {
  using segments_type = basic::Vector<2, const Line*>;
  Line() = default;
  Line(const token::Token* start, const token::Token* end,
       ParseFunctionKind kind, token::details::TokenKind token_kind, size_t len,
       size_t calling_depth, segments_type&& segments)
      : start_(start),
        end_(end),
        kind_(kind),
        token_kind_(token_kind),
        len_(len),
        calling_depth_(calling_depth),
        segments_(std::move(segments)) {}
  const token::Token* start_{nullptr};
  const token::Token* end_{nullptr};
  ParseFunctionKind kind_{ParseFunctionKind::kUnknown};
  token::details::TokenKind token_kind_{token::details::TokenKind::unknown};
  size_t len_{0};
  size_t calling_depth_{0};
  segments_type segments_;
};

inline bool operator==(const Line& a, const Line& b) {
  return a.start_ == b.start_ && a.end_ == b.end_ && a.kind_ == b.kind_ &&
         a.token_kind_ == b.token_kind_ && a.len_ == b.len_ &&
         a.calling_depth_ == b.calling_depth_ && a.segments_ == b.segments_;
}

class Tree {
 public:
  struct Node {
    using sub_nodes_type = std::vector<Node>;
    sub_nodes_type children_;
    const token::Token* start_{nullptr};
    const token::Token* end_{nullptr};
    ParseFunctionKind kind_{ParseFunctionKind::kUnknown};
    token::details::TokenKind token_kind_{token::details::TokenKind::unknown};
    size_t len_{0};
    explicit Node() = default;
    [[nodiscard]] bool valid() const {
      return start_ != nullptr && end_ != nullptr && len_ != 0;
    }
    Node(const Line* line)
        : start_(line->start_),
          end_(line->end_),
          kind_(line->kind_),
          token_kind_(line->token_kind_),
          len_(line->len_) {
      for (const auto* p : line->segments_) {
        children_.push_back(Node(p));
      }
    }
  };
  Tree() = default;
  Tree(Node&& n) : root_(std::move(n)) {}
  explicit Tree(const Line* line) : root_(Node(line)) {}
  [[nodiscard]] const Node& root() const { return root_; }
  [[nodiscard]] basic::Vector<4, const token::Token*> leafs() const {
    using out_type = basic::Vector<4, const token::Token*>;
    out_type out;
    [](const Node& root, out_type& o) -> void {
      auto visit_impl = [](const Node& root, out_type& o,
                           const auto& func) -> void {
        if (root.children_.empty() && root.len_ == 1) {
          o.append(root.start_);
          return;
        }

        for (const auto& child : root.children_) {
          func(child, o, func);
        }
      };

      return visit_impl(root, o, visit_impl);
    }(root_, out);

    return out;
  }

 private:
  Node root_;
};

namespace opt {

class Pass {

 public:
  virtual Tree operator()(const Tree& t) = 0;
};

class SingleNodePathFoldPass : public Pass {
 public:
  Tree operator()(const Tree& in) override {
    return Tree([](const Tree::Node& root) -> Tree::Node {
      auto visit_impl = [](const Tree::Node& root,
                           const auto& func) -> Tree::Node {
        if (root.children_.empty()) {
          return root;
        }

        if (root.children_.size() == 1) {
          return func(root.children_[0], func);
        }
        Tree::Node out = root;
        out.children_.clear();
        for (const auto& child : root.children_) {
          out.children_.push_back(func(child, func));
        }
        return out;
      };

      return visit_impl(root, visit_impl);
    }(in.root()));
  }
};

Tree run(const Tree& in);

}  // namespace opt

}  // namespace lps::parser::details
