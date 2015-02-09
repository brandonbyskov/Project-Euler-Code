#include "problem.h"
#include "problemsupport.h"
#include "fileio.h"
//#include "ttmath/ttmath.h"
#include <iostream>
#include <math.h>
#include <list>
#include <stack>
#include <queue>
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

//find sum of primes up to 'max'
__int64 problem10(int max) {
	if (max < 2) return 0;
	if (max == 2) return 2;

	__int64 sum = 2;

	for (int i = 3; i <= max; i += 2)
	{
		if (isPrime(i)) sum += i;
	}

	return sum;
}

// returns the smallest triangle number with at least n divisors
int problem12(int divisors) {
	if (divisors < 1) return 0;
	int triangle_number = 0;

	for (int i = 1; i > 0; i++) {
		triangle_number += i;
		if (numDivisors(triangle_number) >= divisors)
			return triangle_number;	
	}
	return 0;
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


//returns the starting number with the longest Collatz sequence
int problem14(int max) {
	__int64 n;
	int longest_n = 0;
	int longest_sequence = 0;

	for (int i = 1, j = 1; i < max; i++) {
		j = 1;
		n = i;
		for (; n != 1; j++) {
			if (n % 2 == 0) n /= 2;
			else n = 3*n +1;
		}
		if (j > longest_sequence)
		{
			longest_n = i;
			longest_sequence = j;
		}
	}
	return longest_n;
}

//yay, a recursive function!
// Rest in Peace recursive fuction. Lived from Evening of Jan 11, 2015 - Night of Jan 11, 2015
// One day it went off to do a job, but it never returned :(.


// memoization worked much better than recursion. Algorithms: 1, Brandon: 0;(but not really, cause I solved it)
// Solved prolem using single array, less space required than trangle matrix.
// recursion was too slow.
__int64 problem15(int grid_size) {
	__int64* grid = new __int64 [grid_size+1];
	__int64 num_routes;

	grid[0] = 1;
	for (int i = 1; i <= grid_size; i++) {
		for (int j = 1; j < i; j++) {
			grid[j] += grid[j - 1];
		}
		grid[i] = 2 * grid[i - 1];
	}

	num_routes = grid[grid_size];
	delete [] grid;
	return num_routes;
}

int problem16(int power) {

	int precision = 18;
	int segments = ceil((double)power / 59);
	//array to store all digits
	unsigned long long* num = new unsigned long long [segments];
	for (int i = 0; i < segments; i++) {
		num[i] = 0;
	}

	int carry = 0; // to carry over digit in case of overflow
	long long sum = 0;

	//multiply calculate 2^n
	num[0] = 1;
	for (int i = 1; i <= power; i++) {
		for (int j = 0; j < segments; j++) {
			num[j] *= 2;
			num[j] += carry;
			if (num[j] > 0 && floor(log10(num[j])) + 1 > precision) {
				carry = 1;
				num[j] -= (unsigned long long)pow(10,precision);
			} else { 
				carry = 0;
			}
		}
	}
	//add digits into sum
	for (int i = 0; i < segments; i++) {
		for (int j = 0; j < precision; j++) {
			sum += ((unsigned long long) (num[i] / (long long)pow(10,j)) ) % 10;
		}
	}

	delete [] num;
	return sum;
}

int problem18(string filename, int lines) {
	int** triangle = readTriangle(filename, lines);
	int max_path;
	//cout << "lol";
	if (triangle) {
		//cout << "lol";
		for (int i = lines - 1; i > 0; i--) {
			for (int j = 0; j < i; j++) {
				//cout << triangle[i][]
				triangle[i-1][j] += (triangle[i][j] > triangle[i][j+1]) ? triangle[i][j] : triangle[i][j+1];
			}
			//cout<<'\n';
			delete [] triangle[i];
		}
	}
	max_path = triangle[0][0];
	delete [] triangle[0];
	delete [] triangle;
	return max_path;
}

//finds smallest (x - y) such that x, y, x + y, and x - y are pentagonal
int problem44() {
	int difference = 1000000000;
	__int64 x = 5;
	__int64 y = 1;

	for (int i = 4; i < difference; )
	{		
		for (int j = i - 3; j > 0 && x - y < difference; j -= 3) {
			
			if (isPentagonal(x - y) && isPentagonal(x + y)) {
				difference = x - y;
			}
			y -= j;
		}

		i += 3;
		y = x;
		x += i;
	}

	return difference;
}

// finds the smallest odd composite number that is not equal to the sum of a prime and 2 times a square
int problem46() {
	list<int> primes;
	int temp = 0;
	int solution = 0;
	
	primes.push_back(2);
	for (int i = 3; solution == 0; i += 2)
	{
		if (isPrime(i)) primes.push_back(i);
		else {
			for (list<int>::reverse_iterator rit=primes.rbegin(); temp != i && rit!=primes.rend(); rit++) {
				temp = 0;
				for (int j = 1; i > temp;j++) {

					temp = (*rit) + (2 * j * j);
				}
			}
			if (i < temp) solution = i;
		}
	}

	return solution;
}

//finds last 10 digits of series 1^1 + 2^2 + 3^3 + ... + n^n
__int64 problem48(int max) {
	__int64 sum = 0;
	__int64 power;

	for (int i = 1; i <= max; i++) {
		power = i;
		for (int j = 2; j <= i; j++) {
			power  = (power * i) % 10000000000;

		}
		sum += power;
		sum %= 10000000000;
	}

	return sum;
}

int problem50(int max) {
	if(max < 2) return 0;
	if(max < 5) return 2;
	vector<int> primes;
	int max_solution = 0;

	primes.push_back(2);

	for (int terms = 1, sum = 0; ; terms++) {
		sum = 0;
		//get initial primes
		for(int k = 0; k < terms; k++) {
			if(k < primes.size())
				sum += primes[k];
			else {
				primes.push_back(getNextPrime(primes[k-1]));
				sum += primes[k];
			}
		}
		if (sum > max) break;

		//iterate throught primes, maintaining number of consecutive terms
		for (int i = 0, j = terms; !isPrime(sum) && sum <= max; i++, j++) {
			if(j < primes.size()) {
				sum -= primes[i];
				sum += primes[j];
			} else {
				primes.push_back(getNextPrime(primes[j-1]));
				sum -= primes[i];
				sum += primes[j];
			}
		}
		if (sum <= max) {
			max_solution = (sum > max_solution) ? sum : max_solution;
		}		
	}

	return max_solution;
}

int problem67(string filename, int lines) {
	return problem18(filename, lines);
}

//took 2 minutes, but seems reasonable
int problem92(int max) {
	bool * to89 = new bool [numDigits(max)*81];
	int chain;
	int chain_next;
	int sum= 0;

	for (int i = 1,j = 1; i <= numDigits(max)*81; i++) {
		chain = i;

		while (chain != 89 && chain != 1) {
			chain_next = 0;
			for (int j = 1; j <= numDigits(chain); j++) {
				chain_next += pow(getDigit(chain,j), 2);
			}
			chain = chain_next;
		}
		to89[i-1] = (chain == 89);
	}

	for (int i = 1; i <= max; i++) {
		chain = 0;
		for (int j = 1; j <= numDigits(i); j++) {
			chain += pow(getDigit(i,j), 2);
		}
		sum += to89[chain-1];
	}

	return sum;
}

// This implementation is very slow, primality testing is causeing most of the slowdown. 
// Use the more effiecient prime test algorithm or test for primes less often.
// The modulus stuff is a mess also, I could clean that up with some structs or other workaround.
__int64 problem196(__int64 x, __int64 y) {

	__int64 sum = 0;
	bool** grid = new bool* [5];
	for (int i =0; i < 5; i++) {
		grid[i] = new bool [5];
		for (int j = 0; j < 5; j++) {
			grid[i][j] = false;
		}
	}

	__int64* int_grid = new __int64 [5];

	if (x % 2 == 1) {
		__int64 value = ((x * (x - 1)) / 2) + 1;
		int_grid[0] = value - (2*x-3);
		int_grid[1] = value - (x - 1);
		int_grid[2] = value;
		int_grid[3] = value + (x);
		int_grid[4] = value + (2*x + 1);

		int j;
		for (int i = 0; i < x; i++) {
			if (i%1000 == 0) cout<<i<<'\n';
			j = i % 5;

			grid[(j+2)%5][0] = (i>=x-2)? false : isPrime(int_grid[0]++);
			grid[(j+2)%5][1] = (i>=x-1)? false : isPrime(int_grid[1]++);
			grid[(j+2)%5][2] = isPrime(int_grid[2]++);
			grid[(j+2)%5][3] = isPrime(int_grid[3]++);
			grid[(j+2)%5][4] = isPrime(int_grid[4]++);

			if (grid[j][2] && ( (grid[j][1] && (grid[(j+4)%5][0] || grid[(j+1)%5][0])) || 
								(grid[(j+4)%5][3] && (grid[(j+4)%5][4] || grid[(j+3)%5][2])) || 
								(grid[(j+1)%5][3] && (grid[(j+1)%5][4] || grid[(j+2)%5][2])) ||
								(grid[j][1]+grid[(j+4)%5][3]+grid[(j+1)%5][3] > 1)
								) ) {sum+=int_grid[2]-3;}
		}
	}
	return sum;
}

//Calculates the smallest number to have 2^500500 divisors
__int64 problem500() {
	__int64 prod = 2; // cumulative product
	queue<int> squares; //stores the square of last used value
	squares.push(4);
	int prime = 3;

	for (int i = 2; i <= 500500; i++) {
		if(prime < squares.front()) {
			prod = (prod * prime) % 500500507;
			if (prime < 2716) squares.push(prime*prime);
			prime = getNextPrime(prime);
		} else {
			prod = (prod * squares.front()) % 500500507;
			squares.push(squares.front()*squares.front());
			squares.pop();
		}
	}
	return prod;
}

//TODO: generate list of primes <= 100.
//i has factors 1, a prime, prime || prime*prime, prime || prime*prime, all <sqrt(i)
__int64 problem501(__int64 max, int divisors) {
	if (max < 24) return 0;
	__int64 count = 0;
	__int64 maxPrime = max/4;
	int numPrimes = (int) ( 1.25*(max/4)/log(max/4));
	__int64* primes = new __int64[numPrimes];
	/*for (int i = 0, j = 2; j < maxPrime; i++, j = getNextPrime(j)) {
		primes[i] = j;
		numPrimes = i + 1;
	}*/

	int maxUnderCube = 0;
	for (int i = 0, p = 2; p <= maxPrime; i++) {
		primes[i] = p;
		numPrimes = i + 1;
		p = getNextPrime(p);
		if (maxUnderCube == 0 && p > pow(max,(double)1/3)) maxUnderCube = i;
	}
	count = (maxUnderCube)*(maxUnderCube)*(maxUnderCube-1)-3;
	__int64 d1,d2,d3 = 0;
	__int64 value = 0;
	int temp = 0;
	for (int i = 0;i<maxUnderCube+1;i++) {
		for (int j = i;j<numPrimes;j++) {
			d1 = primes[i];
				if (i == j) d2 = (__int64)primes[j]*d1;
				else d2 = primes[j];
			for (int k = maxUnderCube+1>j?maxUnderCube+1:j;k<numPrimes && primes[k]<=max/(d1*d2);k++) {
				
				if (j == k) d3 = (__int64)primes[k]*d2;
				else d3 = primes[k];
				value = d1*d2*d3;//primes[i]*primes[j]*primes[k];
				
				
				if (value <= max && value > 0) {
					//if (numDivisors(value) == 8 ) cout<<value<<"!!!";
					if (ceil(sqrt(value)) != floor(sqrt(value)) ) {
						//cout<<value<<'\n';
						count++;
					}
				} else {
					if (i==j || j==k) continue;
					else break;
				}
			}
		}
		//numPrimes = temp;
	}

	cout<<"Prime "<<numPrimes<<": "<<primes[numPrimes-1]<<'\n';

	bool primeFlag = true;
	int d[3];
	for (__int64 i = 24, num = 24, d = 0; i <= max; i++, d = 0) {
		/*if (numDivisors(i) == divisors) {
			count += 1;
		}*/

		//A2:
		//if (sqrt(i) == floor(sqrt(i)) ) continue; //square number

		//for (int j = 2; j < sqrt(i) && d <= divisors; j++) {
		//	if (i % j == 0) d += 2; //two divisors, i and x/i
		//}
		//if (d == divisors) count++;

		//A3:
		//if (sqrt(i) == floor(sqrt(i)) ) continue; //square number
		//
		//primeFlag = true;
		//num = i;
		//for (int j = 2, k = 3; j <= sqrt(i) && k > 0; j++) {

		//	if (num % j == 0) {
		//		num /= j;
		//		if (k == 3) d1 = j;
		//		if (k == 2) d2 = j;
		//		k--;
		//		if (k == 1) {
		//			j++;
		//			if (num == j) {
		//				count++;
		//				//cout << i <<'\t'<<numDivisors(i)<<'\n';
		//				break;
		//			}
		//			if (( num < j) ) break;
		//			if (num % j == 0) break;
		//			for( ;j <= num/2; j += 1) {
		//				if (num % (j) == 0) {
		//					if (i == num * j) {}
		//					else {
		//						primeFlag = false;
		//						break;
		//					}
		//				}
		//			}
		//			if (primeFlag == true) {count++;}//cout << i <<'\t'<<numDivisors(i)<<'\n';}
		//			k--;
		//		}
		//		//if(k == 0 && num == 1) count++;
		//			
		//		
		//	}
		//}

		//A4 incomplete:
		/*num = i;
		for (int j = 0; j < numPrimes && primes[j] < sqrt(i); j++) {
			if (num % primes[j] == 0) {
				num /= i;
				;
			}

		}*/

		
	}
	return count;
}