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

```
proyecto/
├── 39_Pointers/                 - Conceptos básicos de punteros
│   ├── Pointers.cpp            - Código fuente
│   └── build/                  - Directorio de compilación (generado automáticamente)
│       └── Pointers           - Ejecutable (generado al compilar)
├── 40_Arithmetic/              - Aritmética de punteros
├── 41_2Pointers and Arrays/    - Punteros y arreglos
└── ... otros directorios
```

Cada tutorial está en su propio directorio y cuando se compila, se crea automáticamente una carpeta `build/` que contiene los ejecutables. Esta carpeta `build/` está excluida de git para mantener limpio el repositorio.

## Compilación y Ejecución

### Usando VSCode (Método Recomendado)
1. Abre la carpeta del tutorial en VSCode
2. Abre el archivo `.cpp` que deseas ejecutar
3. Presiona `Ctrl+Shift+B` (Windows) o `Cmd+Shift+B` (macOS)
4. ¡Listo! El programa se compilará y ejecutará automáticamente

El sistema:
- Creará automáticamente una carpeta `build/` en el directorio del archivo fuente
- Compilará y colocará el ejecutable dentro de esta carpeta
- Ejecutará el programa mostrando la salida en un terminal nuevo

### Compilación Manual (Alternativa)

Si prefieres usar la terminal directamente:

#### En Windows
```bash
cd <directorio_del_ejemplo>
mkdir build
g++ archivo.cpp -o build/archivo.exe && ./build/archivo.exe
```

#### En macOS
```bash
cd <directorio_del_ejemplo>
mkdir -p build
g++-14 archivo.cpp -o build/archivo && ./build/archivo
```

## Estructura de Archivos de Configuración

### .gitignore
- Excluye todas las carpetas `build/` en cualquier nivel del proyecto
- Ignora archivos objeto (*.o, *.obj) y ejecutables (*.exe, *.out)
- Mantiene el repositorio limpio de archivos binarios

## Solución de Problemas

### Problemas Comunes
1. Si el comando g++ no funciona:
   - Windows: Verifica que MinGW esté en el PATH (C:\msys64\mingw64\bin)
   - macOS: Verifica que g++-14 esté instalado con `brew list | grep gcc`

2. Si obtienes errores de compilación:
   - Verifica que estés en el directorio correcto
   - Asegúrate de que todos los archivos necesarios estén presentes

3. Si VSCode no detecta el compilador:
   - Windows: Reinicia VSCode después de agregar MinGW al PATH
   - macOS: Verifica que la ruta a g++-14 sea correcta (/opt/homebrew/bin/g++-14)

4. Si no se crea la carpeta build:
   - Verifica que tengas permisos de escritura en el directorio
   - Intenta crear la carpeta manualmente y verifica los permisos 