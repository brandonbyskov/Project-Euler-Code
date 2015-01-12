#include "problem.h"
#include "problemsupport.h"
//#include "ttmath/ttmath.h"
#include <iostream>
#include <math.h>
#include <list>
#include <stdlib.h>
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

int problem6(int max) {
	if (max <= 0) return 0;

	int sum_squares = 0;
	int squared_sum = 0;

	for (int i = 1; i <= max; i++) {
		sum_squares += i*i;
		squared_sum +=i;
	}
	squared_sum *= squared_sum;

	return squared_sum - sum_squares;
}

int problem7(int n) {
	if (n <= 0) return false;
	if (n == 1) return 2;

	int j = 3;
	for (int i = 2; i <= n; j += 2) {
		if (isPrime(j)) {
			i++;
		}
	}

	return j - 2;
}

__int64 problem8(string num, int adjacent) {
	if (adjacent < 1 || num.length() < adjacent) return 0;
	__int64 max_product = 0;
	__int64 temp_product = 1;
	char digit;

	for (int i = 0, j = 0; j < num.length();) {

		//reset, count next adjacent digits
		if (i == j) {
			for (; j < i+adjacent && j < num.length(); ) {
				if (atoi( &(digit = num[j]) ) == 0) {
					 j++;
					 i = j;
					temp_product = 1;
				} else {
					temp_product *= atoi( &(digit = num[j]) );
					j++;
				}
			}
			max_product = (temp_product > max_product && j == i + adjacent)? temp_product : max_product;
			
		} else {
		
			//reset if num[j] == 0
			if (atoi( &(digit = num[j])) == 0) {
				i = j = j+1;
				temp_product = 1;
			} else { //else, iterate one digit at a time
				temp_product /= atoi( &(digit = num[i]) );
				temp_product *= atoi( &(digit = num[j]) );
				max_product = (temp_product > max_product)? temp_product : max_product;
				i++;
				j++;
			}
		}	
	}

	return max_product;
}

int problem9(int sum) {
	if (sum < 12) return 0;
	int c = sum / 2;
	int remaining;
	int a = 0;
	int b = 0;

	while (a*a + b*b != c*c && c > ceil((double)sum/3)) {
		c--;
		remaining = sum - c;

		if (remaining % 2 == 0) {
			a = (remaining / 2) -1;
			b = (remaining / 2) +1;
		} else {
			a = remaining / 2;
			b = (remaining / 2) +1;
		}

		while (a*a + b*b < c*c && a>1 && b<c-1) {
			a--;
			b++;
		}

	}

	if (a*a + b*b == c*c) return a*b*c;
	else return 0;
}

//find sum of primes below 'max'
__int64 problem10(int max) {
	if (max < 3) return 0;
	if (max == 3) return 2;

	__int64 sum = 2;

	for (int i = 3; i < max; i += 2)
	{
		if (isPrime(i)) sum += i;
	}

	return sum;
}

//returns the first 10 significant digits of the sum of many large numbers
__int64 problem13(string num, int count, int size) {
	if (num.length() != count*size || count < 1 || size < 1) return 0;

	__int64 sum = 0;
	int segments = ceil ((double)size / 9);
	int first_segment = size % 9; //size of first number segment

	// iterate over n segments of size 9 digits each (except for first, which may be smaller than 9)
	for (int i = 0, start = size, seg_size; i < segments; i++) {
		sum /= 1000000000; //throw away insignificant digits
		seg_size = (i == 0)? first_segment : 9;
		start = size - first_segment - 9*i;

		for (int j = 0; j < count; j++) {
			sum += atoi( ((num.substr(start+j*size, seg_size).c_str())) );		
		}
	}

	//keep only 10 significant digits
	sum /= pow(10,floor(log10(sum)) + 1 - 10);
	return sum;
}