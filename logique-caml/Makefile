CMPL=ocamlc # Compilateur
EXEC=satsol # Les executables à générer

BUILD_DIR=build
SOURCE_DIR=.

OBJ=$(BUILD_DIR)/trilean.cmo \
	$(BUILD_DIR)/cnf.cmo \
	$(BUILD_DIR)/naif.cmo \
	$(BUILD_DIR)/main.cmo

INC=$(BUILD_DIR)/trilean.cmi \
	$(BUILD_DIR)/cnf.cmi \
	$(BUILD_DIR)/naif.cmi


all: satsol

satsol: $(INC) $(OBJ)
	ocamlc -g -o $(EXEC) $(OBJ)

clean:
	rm -rf build/
	rm $(EXEC)

$(BUILD_DIR)/%.cmi: $(SOURCE_DIR)/%.mli
	@mkdir -p $(BUILD_DIR)
	ocamlc -c -o $@ $(SOURCE_DIR)/$*.mli -I $(BUILD_DIR)

$(BUILD_DIR)/%.cmo: $(SOURCE_DIR)/%.ml
	@mkdir -p $(BUILD_DIR)
	ocamlc -c -o $@ $(SOURCE_DIR)/$*.ml -I $(BUILD_DIR)
