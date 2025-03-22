# Tutorial de C++ - Punteros Completo

Este repositorio contiene una colección de tutoriales de C++ enfocados en punteros y conceptos relacionados. El proyecto ha sido configurado para usar con Visual Studio Code y g++ tanto en Windows como en macOS.

## Prerequisitos

### Para Usuarios de Windows
1. Visual Studio Code
2. Extensión C/C++ para VSCode
3. MinGW-w64 (para g++)
   - Descarga e instala desde: https://www.msys2.org/
   - Después de instalar, abre MSYS2 y ejecuta:
     ```bash
     pacman -S mingw-w64-x86_64-gcc mingw-w64-x86_64-gdb
     ```
   - Asegúrate de agregar la ruta del compilador al PATH (típicamente C:\msys64\mingw64\bin)

### Para Usuarios de macOS
1. Visual Studio Code
2. Extensión C/C++ para VSCode
3. GCC/g++ (instalar vía Homebrew)
   ```bash
   brew install gcc
   ```
   Esto instalará la última versión de g++ (actualmente g++-14)

## Estructura del Proyecto

Cada tutorial está en su propio directorio con un archivo .cpp que contiene el código del ejemplo:

```
39_Pointers/                    - Conceptos básicos de punteros
  ├─ main.cpp                   - Código del ejemplo
  └─ README.md                  - Explicación del concepto
40_Arithmetic/                  - Aritmética de punteros
41_2Pointers and Arrays/       - Punteros y arreglos
42_Pointer Arithmetic/         - Aritmética avanzada de punteros
43_CharArrays/                 - Arreglos de caracteres
44_ReversingString/           - Manipulación de cadenas
45_References/                - Referencias en C++
46_Const/                     - Palabra clave const
47_Copy Constructors/         - Constructores de copia
48_New Operator/              - Asignación dinámica de memoria
49_Returning Objects/         - Retorno de objetos desde métodos
50_Allocating Memory/         - Asignación de memoria
51_Arrays and Functions/      - Arreglos con funciones
52_Namespaces/               - Espacios de nombres en C++
```

## Compilación y Ejecución

### Opción 1: Ejecutar archivos .cpp directamente (Recomendado)

Esta es la forma más simple de ejecutar los ejemplos:

#### En Windows
```bash
# Compilar y ejecutar en un solo paso
cd 39_Pointers
g++ Pointers.cpp -o Pointers && ./Pointers
```

#### En macOS
```bash
# Compilar y ejecutar en un solo paso
cd 39_Pointers
g++-14 Pointers.cpp -o Pointers && ./Pointers
```

### Opción 2: Usar VSCode

1. Abre la carpeta del tutorial en VSCode (ejemplo: `39_Pointers`)
2. Abre el archivo `main.cpp`
3. Presiona F5 para compilar y ejecutar directamente

## Solución de Problemas

### Problemas Comunes
1. Si el comando g++ no funciona:
   - Windows: Verifica que MinGW esté en el PATH (C:\msys64\mingw64\bin)
   - macOS: Usa g++-14 en lugar de g++

2. Si obtienes errores de compilación:
   - Verifica que estés en el directorio correcto
   - Asegúrate de incluir todos los archivos necesarios en el comando de compilación

3. Si la depuración no funciona en VSCode:
   - Windows: Verifica que gdb esté instalado
   - macOS: Instala gdb con `brew install gdb` 