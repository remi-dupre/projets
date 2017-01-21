CMPL=ocamlc # Compilateur
EXEC=satsol # Les executables à générer

BUILD_DIR=build
SOURCE_DIR=.

SRC=$(wildcard $(SOURCE_DIR)/*.ml) # La liste des .cpp
OBJ=$(patsubst $(SOURCE_DIR)/%.ml,$(BUILD_DIR)/%.cmo,$(SRC))

SRC_INC=$(wildcard $(SOURCE_DIR)/*.mli)
INC=$(patsubst $(SOURCE_DIR)/%.mli,$(BUILD_DIR)/%.cmi,$(SRC_INC))



all: satsol

satsol: $(INC) $(OBJ)
	ocamlc -g -o $(EXEC) $(OBJ)

clean:
	rm -rf build/
	rm $(EXEC)

$(BUILD_DIR)/%.cmi: $(SOURCE_DIR)/%.mli
	@mkdir -p $(BUILD_DIR)
	ocamlc -c -o $@ $(SOURCE_DIR)/$*.mli

$(BUILD_DIR)/%.cmo: $(SOURCE_DIR)/%.ml
	@mkdir -p $(BUILD_DIR)
	ocamlc -c -o $@ $(SOURCE_DIR)/$*.ml -I $(BUILD_DIR)
