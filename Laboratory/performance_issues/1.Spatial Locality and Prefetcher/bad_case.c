
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define ONE_K 1024
#define ONE_M ONE_K * ONE_K
#define VSIZE (500 * ONE_M)
#define ITERS (10 * ONE_M)

int main() {
  srand(time(NULL));

  // Vector de 100 MB
  unsigned char *vec = malloc(VSIZE);
  for (int i = 0; i < VSIZE; i++) {
    vec[i] = rand() % 256;
  }

  // Acceso aleatorio causa mal uso de caché (sin localidad espacial)
  clock_t start = clock();
  unsigned int sum = 0;
  for (int i = 0; i < ITERS; i++) {
    int pos = rand() % VSIZE;
    if (pos > VSIZE)
      printf("Imposible. Solo para forzar mantener pos.\n");
    else
      sum += vec[pos];
  }

  double elapsedTime = (double)(clock() - start) / CLOCKS_PER_SEC;
  printf("Tiempo total: %lf\n", elapsedTime);
  return 0;
}
