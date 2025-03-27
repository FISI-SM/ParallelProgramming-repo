//============================================================================
// Name        : Pointer.cpp
// Author      : John Purcell
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
using namespace std;

int main()
{

  const int NSTRINGS = 5; //
  string texts[5] = {"one", "two", "three", "four", "five"};
  string *pTexts = texts;

  pTexts += 3;
  cout << *pTexts << endl; // "four"
  pTexts -= 2;
  cout << *pTexts << endl; // "two"

  string *pEnd = &texts[NSTRINGS];
  pTexts = &texts[0];

  while (pTexts != pEnd)
  {
    cout << *pTexts << endl; // "five"
    pTexts++;
  }

  // Set pTexts back to start.
  pTexts = &texts[0];

  long elements = (long)(pEnd - pTexts);

  cout << elements << endl; // 5

  // Set pTexts back to start.
  pTexts = &texts[0];

  pTexts += NSTRINGS / 2;
  cout << *pTexts << endl; // "threee"

  return 0;
}