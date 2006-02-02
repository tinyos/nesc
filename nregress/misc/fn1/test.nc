#include "fun.h"
module test
{
}
implementation
{
  void silly();

  int main() __attribute__((C, spontaneous)) {
    silly();
  }
}
