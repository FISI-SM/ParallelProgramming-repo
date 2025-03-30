//============================================================================
// Name        : New.cpp
// Author      : John Purcell
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
using namespace std;

class Animal
{
private:
  string name;

public:
  Animal()
  {
    cout << "Animal created." << endl;
  }

  Animal(const Animal &other) : name(other.name)
  {
    cout << "Animal created by copying." << endl;
  }

  ~Animal()
  {
    cout << "Destructor called" << endl;
  }

  void setName(string name)
  {
    this->name = name;
  }

  void speak() const
  {
    cout << "My name is: " << name << endl;
  }
};

int main()
{
  // Lives on the stack
  Animal animal;
  // Lives on the heap
  Animal *pCat1 = new Animal();
  pCat1->setName("Freddy");
  pCat1->speak();
  // (*pCat1).speak();
  delete pCat1;

  // 8 bytes in 64-bits
  // 4 bytes in 32-bits
  cout << sizeof(pCat1) << endl;

  return 0;
}