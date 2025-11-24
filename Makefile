BIN_DIR := bin

# Busca todos los archivos .hs dentro de src/
SRC := $(wildcard *.hs)

TARGET := $(BIN_DIR)/juego
GHC_FLAGS := -O2 -package gloss -package gloss-juicy

.PHONY: all clean run dirs

all: dirs $(TARGET)

dirs:
	mkdir -p $(BIN_DIR)

$(TARGET): $(SRC)
	ghc $(GHC_FLAGS) -outputdir $(BIN_DIR) -o $(TARGET) $(SRC)

run: all
	./$(TARGET)

clean:
	rm -rf $(BIN_DIR)/*
