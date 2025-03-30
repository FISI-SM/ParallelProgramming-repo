#include <iostream>

using namespace std;

int *encontrarMaximo(int *valores, int tam)
{
  int *max = valores;

  for (int i = 0; i < tam; i++)
  {
    if (valores[i] > *max)
    {
      max = &valores[i];
    }
  }
  return max;
}

int main()
{
  int valores[] = {10, 50, 20, 40, 5};
  int tam = sizeof(valores) / sizeof(int);

  int *max = encontrarMaximo(valores, tam);

  *max = 999;

  for (int i = 0; i < tam; i++)
  {
    cout << valores[i] << endl;
  }

  return 0;
}
