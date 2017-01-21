CMPL=ocamlopt # Compilateur
EXEC=satsol # Les executables à générer
FLAGS=-O3

BUILD_DIR=build
SOURCE_DIR=.

OBJ=$(BUILD_DIR)/tools.cmx \
	$(BUILD_DIR)/trilean.cmx \
	$(BUILD_DIR)/cnf.cmx \
	$(BUILD_DIR)/naif.cmx \
	$(BUILD_DIR)/dpll.cmx \
	$(BUILD_DIR)/input.cmx \
	$(BUILD_DIR)/main.cmx

INC=$(BUILD_DIR)/tools.cmi \
	$(BUILD_DIR)/trilean.cmi \
	$(BUILD_DIR)/cnf.cmi \
	$(BUILD_DIR)/naif.cmi \
	$(BUILD_DIR)/dpll.cmi \
	$(BUILD_DIR)/input.cmi


all: satsol

satsol: $(INC) $(OBJ)
	$(CMPL) -g -o $(EXEC) $(OBJ) $(FLAGS)

clean:
	rm -rf build/
	rm $(EXEC)

$(BUILD_DIR)/%.cmi: $(SOURCE_DIR)/%.mli
	@mkdir -p $(BUILD_DIR)
	$(CMPL) -c -o $@ $(SOURCE_DIR)/$*.mli -I $(BUILD_DIR) $(FLAGS)

$(BUILD_DIR)/%.cmx: $(SOURCE_DIR)/%.ml
	@mkdir -p $(BUILD_DIR)
	$(CMPL) -c -o $@ $(SOURCE_DIR)/$*.ml -I $(BUILD_DIR) $(FLAGS)
