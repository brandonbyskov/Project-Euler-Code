#include "problemsupport.h"
#include <iostream>
#include <math.h>
using namespace std;

bool isPalindrome(int x) {
	if (x < 0) return false;

	int x_digits = floor(log10(x)) + 1;
	int i_digit;
	int j_digit;
	
	for (int i = 0, j = x_digits - 1; i < j; i++, j--) {
		i_digit = ((int) (x / pow(10,i)) ) % 10;
		j_digit = ((int) (x / pow(10,j)) ) % 10;

		if (i_digit != j_digit) return false;
	}

	return true;
}

bool isPrime(__int64 x) {
	if (x < 1) return false;
	for (__int64 i = 2; i <= sqrt(x); i++) {
		if (x % i == 0) return false;
	}
	return true;
}