#include <mpi.h>
#include <stdio.h>

#define ARRAY_SIZE 5

int main(int argc, char *argv[])
{
    int my_rank, comm_sz;   

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &comm_sz);

    int data[ARRAY_SIZE] = {0, 1, 2, 3, 4}; // Declaración inicial

    // Cada proceso modifica su copia
    for (int i = 0; i < ARRAY_SIZE; i++)
        data[i] *= my_rank;

    // Imprimir el resultado
    printf("Proceso %d: arreglo modificado = ", my_rank);
    for (int i = 0; i < ARRAY_SIZE; i++)
        printf("%d ", data[i]);
    printf("\n");

    MPI_Finalize();

    return 0;
}
