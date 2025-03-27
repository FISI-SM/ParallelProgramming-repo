//============================================================================
// Name        : Pointers.cpp
// Author      : John Purcell
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
using namespace std;

void manipulate(double *pValue)
{
	cout << "2. Value of double in manipulate: " << *pValue << endl; // 123.4
	*pValue = 10.0;
	cout << "3. Value of double in manipulate: " << *pValue << endl; // 10.0
}

int main()
{

	int nValue = 8;

	int *pnValue = &nValue;
	// *pnValue = 6;

	cout << "Int value: " << nValue << endl;							 // 8
	cout << "Pointer to int address: " << pnValue << endl; // 0xabc
	cout << "Int value via pointer: " << *pnValue << endl; // 8

	cout << "==================" << endl;

	double dValue = 123.4;

	cout << "1. dValue: " << dValue << endl; // 123.4
	manipulate(&dValue);
	cout << "4. dValue: " << dValue << endl; // 10

	return 0;
}
