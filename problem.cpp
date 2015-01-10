#include "problem.h"
#include "problemsupport.h"
#include <iostream>
#include <math.h>
#include <list>
using namespace std;

int problem1(int x, int y, int max) {
	int product = x*y;
	if (x <= 0 || y <= 0 || max <= 0) return 0;
	int sum = 0;
	list<int> multiples;

	for (int i = 1; i <= product; i++) {
		if (i % x == 0 || i % y == 0) {
			multiples.push_back(i);
		}
	}
	int iter = 0; //iterator from 0 to max
	if (max >= product) {
		for (; iter + product < max ; iter += product) {
			for (list<int>::iterator it=multiples.begin(); it != multiples.end(); ++it) {
				cout << iter+*it;
				cout << '\n';
				sum += iter+*it;				
			}
		}
	}

	for (list<int>::iterator it=multiples.begin(); it != multiples.end(); ++it) {
		if (iter+*it < max) {
			cout << iter+*it;
			cout << '\n';
			sum += iter+*it;
		}
	}



	for (int i = 0;;) 
	return sum;
}

int problem2(int multiple, int max) {
	if (max < 1 || multiple < 1) return 0;
	int sum = 0;

	for ( int i = 1, j = 1, temp = 0; i <= max; ) {
		if (i % multiple == 0) {
			sum += i;
		}
		temp = i;
		i += j;
		j = temp;

	}

	return sum;
}

__int64 problem3(__int64 num) {
	if (num < 1) return 0;
	__int64 factor = 0;
	for (__int64 i = 2; i <= sqrt(num) && factor < i; i++) {
		if (num % i == 0) {
			if (isPrime(i)) factor = i;
			if (isPrime(num/i))factor = num/i;
		}

	}
	return factor;
}

int problem4(int digits) {
	if (digits < 1) return 0;

	int min = pow(10,digits-1);
	int max = pow(10,digits)-1;

	int product;
	int palindrome = 0;
	for (int i = max; i >= min; i--) {
		max = i;
		for (int j = i; j >= min; j--) {
			product = i*j;
			if (product > palindrome) {
				if (isPalindrome(product)) {
					cout<< product;
					cout << '\n';
					palindrome = product;
					i--;
					min = j;
					j = i;
				}
			} else {
				i--;
				j = i;
			}
		}
	}
	return palindrome;

}

int problem5(int max_divisor) {
	int min_divisor = (max_divisor / 2) + 1;

	for (int i = max_divisor; i > 0; i += max_divisor) {
		for (int j = max_divisor; j >= min_divisor; j--) {
			if (i % j == 0 && j == min_divisor) return i;
			if (i % j != 0) {
				break;
			}
		}
	}
	return 0;
}