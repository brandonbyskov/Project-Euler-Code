#include "fileio.h"
#include <iostream>
#include <fstream>
using namespace std;

int** readTriangle(string filename, int lines) {
	int** triangle = new int* [lines];
	ifstream file;
	char str[8];
	int* line_values;

	file.open(filename);
	if(file.is_open()) {
		for (int i = 0; i < lines; i++) {
			line_values = new int [i+1];
			//file.getline(str,3,' ');
			for (int j = 0; j < i; j++) {
				
				file.getline(str,3,' ');
				//cout << str<<' ';
				line_values[j] = atoi(str);

			}
			file.getline(str,3);
			line_values[i] = atoi(str);
			triangle[i] = line_values;
			//cout<<str<<'\n';
		}
	}
	return triangle;
}