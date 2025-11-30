# Makefile para el proyecto Haski

# Nombre del ejecutable
EXEC = juego

# Directorio de salida
BIN_DIR = bin

# Paquetes necesarios
PACKAGES = -package gloss \
           -package gloss-juicy \
           -package JuicyPixels \
           -package aeson \
           -package bytestring \
           -package filepath \
           -package vector \
           -package unordered-containers \
           -package text \
           -package containers \
           -package array \
           -package scientific \
           -package mtl\
           -package sdl2-mixer\
           -package sdl2

# Flags de compilación
GHC_FLAGS = -odir $(BIN_DIR) -hidir $(BIN_DIR) -o $(BIN_DIR)/$(EXEC)

# Regla principal
all: build

# Crear directorio bin si no existe
$(BIN_DIR):
	mkdir -p $(BIN_DIR)

# Instalar dependencias con cabal
install-deps:
	cabal install --lib gloss gloss-juicy JuicyPixels aeson bytestring filepath vector unordered-containers text containers array scientific mtl

# Compilar el proyecto
build: $(BIN_DIR)
	ghc $(GHC_FLAGS) $(PACKAGES) --make main.hs

# Ejecutar el juego
run: build
	./$(BIN_DIR)/$(EXEC)

# Limpiar archivos compilados
clean:
	rm -rf $(BIN_DIR)/*.o $(BIN_DIR)/*.hi $(BIN_DIR)/$(EXEC)

# Limpiar todo
cleanall:
	rm -rf $(BIN_DIR)

.PHONY: all build run clean cleanall install-deps