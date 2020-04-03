#include <stdio.h>
#include "test_api/c/arith.h"

int main(){
  int num1 = 3;
  int num2 = 3;
  printf("Sum of %d, %d = %d\n", num1, num2, adder(num1, num2));
  printf("Subtraction of %d, %d = %d\n", num1, num2, subtractor(num1, num2));
  printf("Product of %d, %d = %d\n", num1, num2, multiplier(num1, num2));
  printf("Division of %d, %d = %d\n", num1, num2, divisor(num1, num2));
  return 0;
}
