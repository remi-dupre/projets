CMPL=ocamlc # Compilateur
EXEC=satsol # Les executables à générer

FLAGS=

BUILD_DIR=build
SOURCE_DIR=.

SRC=$(wildcard $(SOURCE_DIR)/*.ml) # La liste des .cpp
OBJ=$(patsubst $(SOURCE_DIR)/%.ml,$(BUILD_DIR)/%.cmo,$(SRC))

SRC_INC=$(wildcard $(SOURCE_DIR)/*.mli)
INC=$(patsubst $(SOURCE_DIR)/%.mli,$(BUILD_DIR)/%.cmi,$(SRC_INC))


$(SOURCE_DIR)/%.ml: $(BUILD_DIR)/%.cmo
	@mkdir -p $(BUILD_DIR)
	ocamlc -g -c $<

all: satsol

satsol: $(INC) $(OBJ)
	ocamlc -g -o satsol $(OBJ)

clean:
	rm -rf build/
	rm $(EXEC)

makedir:
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/%.cmi: makedir
	ocamlc -c -o $@ $(SOURCE_DIR)/$*.mli

$(BUILD_DIR)/%.cmo: makedir
	ocamlc -c -I $(BUILD_DIR) -o $@ $(SOURCE_DIR)/$*.ml