//============================================================================
// Name        : Pointers.cpp
// Author      : John Purcell
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
using namespace std;

// Loop through an array using a pointer, with array element reference syntax
// Loop through an array by incrementing a pointer
// Loop through an array, stopping by comparing two pointers

int main()
{

  string texts[] = {"one", "two", "three"};
  string *pTexts = texts; // &texts[0]

  for (int i = 0; i < sizeof(texts) / sizeof(string); i++)
  {
    cout << pTexts[i] << " " << flush; // "one", "two", "three"
  }
  cout << endl;

  for (int i = 0; i < sizeof(texts) / sizeof(string); i++, pTexts++)
  {
    cout << pTexts << " ";           // 0xabc
    cout << *pTexts << " " << flush; // "one"
    cout << "\n";
  }

  cout << endl;

  string *pElement = &texts[0];
  string *pEnd = &texts[2];

  while (true)
  {
    cout << *pElement << " " << flush; // "one"

    if (pElement == pEnd)
    {
      break;
    }

    pElement++;
  }

  return 0;
}