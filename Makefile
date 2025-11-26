.PHONY: all build run clean

# Configuración
GHC = ghc
GHCFLAGS = -O2 -threaded -outputdir bin -o bin/juego -package mtl -package gloss -package array
SOURCES = main.hs types.hs logic.hs render.hs
EXECUTABLE = bin/juego

# Regla por defecto
all: build

# Crear el directorio bin si no existe
bin:
	@mkdir -p bin

# Compilar el juego
build: bin $(SOURCES)
	$(GHC) $(GHCFLAGS) --make main.hs

# Ejecutar el juego
run: build
	./$(EXECUTABLE)

# Limpiar archivos generados
clean:
	rm -rf bin

# Instalar dependencias (requiere cabal)
install-deps:
	cabal install --lib gloss
	cabal install --lib mtl
	cabal install --lib gloss-juicy
	cabal install --lib JuicyPixels