
#include <stdio.h>

void incrementar(int *ptr, int tam)
{
  for (int i = 0; i < tam; i++)
  {
    *(ptr + i) += 1;
  }
}

int main()
{
  int numeros[] = {3, 6, 9, 12, 15};
  int tam = sizeof(numeros) / sizeof(int);

  incrementar(numeros, tam);

  for (int i = 0; i < tam; i++)
  {
    printf("%d", numeros[i]);
  }

  return 0;
}