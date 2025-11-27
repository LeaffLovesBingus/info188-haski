.PHONY: all build run clean

# Configuración
GHC = ghc
GHCFLAGS = -O2 -threaded -outputdir bin -o bin/juego -package mtl -package gloss -package array -package aeson -package bytestring -package scientific -package vector -package unordered-containers -package text -package filepath
SOURCES = main.hs Types.hs Logic.hs Render.hs Assets.hs MapLoader.hs
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
	cabal install gloss --lib
	cabal install mtl --lib
	cabal install gloss-juicy --lib
	cabal install JuicyPixels --lib
	cabal install aeson --lib