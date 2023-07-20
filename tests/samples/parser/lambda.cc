template <auto F>
bool func(int a, int b) {
  return F(a, b);
}

int main() {
  auto r = func<[](int a, int b) {
    return a > b;
  }>(1, 2);
}
