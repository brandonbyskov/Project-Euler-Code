#include "problemsupport.h"
#include <iostream>
#include <math.h>
using namespace std;

int getDigit(int x, int pos) {
	return ((int) (x / pow(10,pos-1)) ) % 10;
}

int numDigits(int x) {
	return floor(log10(x)) + 1;
}

int numDivisors(__int64 x) {
	if (x < 0) return 0;
	if (x == 1) return 1;

	int count = 0;
	if (sqrt(x) == floor(sqrt(x))) count++; //square number

	for (int i=1; i < sqrt(x); i++) {
		if (x % i == 0) count += 2; //two divisors, i and x/i
	}
	return count;
}

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

bool isPentagonal(__int64 x) {
	if (x <= 0) return false;

	__int64 i = ceil(sqrt((2*x)/3));
	if( x == i*(3*i - 1)/2) return true;

	return false;
}

bool isPrime(__int64 x) {
	if (x < 2) return false;
	if (x == 2 || x == 3) return true;
	if (x % 2 == 0 || x % 3 == 0) return false;
	for (int i = 5; i <= sqrt(x); i += 6) {
		if (x % (i) == 0 || x % (i + 2) == 0) return false;
	}
	return true;
}

int getNextPrime(int x) {
	if (x < 2) return 2;

	if (x % 2 == 0) x +=1;
	else x += 2;

	while (!isPrime(x)) {
		x += 2;
	}

	return x;
}