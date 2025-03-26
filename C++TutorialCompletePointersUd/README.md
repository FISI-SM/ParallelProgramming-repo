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

### Usando VSCode

#### Compilar y Ejecutar (Sin Depuración)
1. Abre la carpeta del tutorial en VSCode
2. Abre el archivo `.cpp` que deseas ejecutar
3. Presiona `Ctrl+Shift+B` (Windows) o `Cmd+Shift+B` (macOS) para compilar
4. Para ejecutar, presiona `Ctrl+Shift+P` (Windows) o `Cmd+Shift+P` (macOS) y selecciona "Run Task", luego elige "C++: Run Program"

#### Depurar el Programa
1. Abre el archivo `.cpp` que deseas depurar
2. Establece puntos de interrupción haciendo clic en el margen izquierdo donde deseas detener la ejecución
3. Presiona `F5` para iniciar la depuración
4. Utiliza los controles de depuración para paso a paso, continuar, etc.

El sistema:
- Creará automáticamente una carpeta `build/` en el directorio del archivo fuente
- Compilará con símbolos de depuración (-g) y colocará el ejecutable dentro de esta carpeta
- Utilizará la herramienta de depuración adecuada para tu sistema (lldb en macOS, gdb en Windows)

## Extensiones Recomendadas para VSCode

Para una mejor experiencia de desarrollo en C++, asegúrate de tener instalada la extensión oficial del lenguaje:

- [C/C++ Extension Pack (Microsoft)](https://marketplace.visualstudio.com/items?itemName=ms-vscode.cpptools-extension-pack)  
  Incluye:
  - Soporte completo para IntelliSense
  - Depuración (gdb/lldb)
  - Explorador de símbolos
  - Formateo de código y más


### Compilación Manual (Alternativa)

Si prefieres usar la terminal directamente:

#### En Windows
```bash
cd <directorio_del_ejemplo>
mkdir build
g++ -g archivo.cpp -o build/archivo.exe && ./build/archivo.exe
```

#### En macOS
```bash
cd <directorio_del_ejemplo>
mkdir -p build
g++-14 -g archivo.cpp -o build/archivo && ./build/archivo
```

## Estructura de Archivos de Configuración

### .vscode/tasks.json
Define tareas para:
- Compilar programas C++ con símbolos de depuración
- Ejecutar programas compilados sin depuración

### .vscode/launch.json
Define configuraciones para:
- Depurar programas C++ en macOS (usando lldb)
- Depurar programas C++ en Windows (usando gdb)

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

4. Si la depuración no funciona correctamente:
   - Verifica que el programa se compile con la bandera `-g` (incluida en las tareas configuradas)
   - macOS: Asegúrate de que lldb esté instalado y funcione correctamente
   - Windows: Verifica que gdb esté instalado (`pacman -S mingw-w64-x86_64-gdb` en MSYS2)

5. Si no se crea la carpeta build:
   - Verifica que tengas permisos de escritura en el directorio
   - Intenta crear la carpeta manualmente y verifica los permisos 