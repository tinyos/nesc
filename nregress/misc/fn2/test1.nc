#include "fun.h"
module test1
{
}
implementation
{
  void silly();

  int main() __attribute__((C, spontaneous)) {
    silly();
  }
}
