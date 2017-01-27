CMPL=ocamlopt # Compilateur
EXEC=satsol # Les executables à générer
FLAGS=

BUILD_DIR=build
SOURCE_DIR=.

OBJ=$(BUILD_DIR)/tools.cmx \
	$(BUILD_DIR)/trilean.cmx \
	$(BUILD_DIR)/cnf.cmx \
	$(BUILD_DIR)/naif.cmx \
	$(BUILD_DIR)/dpll.cmx \
	$(BUILD_DIR)/input.cmx \
	$(BUILD_DIR)/picross.cmx \
	$(BUILD_DIR)/main.cmx

INC=$(BUILD_DIR)/tools.cmi \
	$(BUILD_DIR)/trilean.cmi \
	$(BUILD_DIR)/cnf.cmi \
	$(BUILD_DIR)/naif.cmi \
	$(BUILD_DIR)/dpll.cmi \
	$(BUILD_DIR)/picross.cmi \
	$(BUILD_DIR)/input.cmi


all: $(EXEC)

$(EXEC): $(INC) $(OBJ)
	$(CMPL) $(FLAGS) -I $(BUILD_DIR) -o $(EXEC) $(OBJ)

clean:
	rm -rf build/
	rm $(EXEC)

$(BUILD_DIR)/%.cmi: $(SOURCE_DIR)/%.mli
	@mkdir -p $(BUILD_DIR)
	$(CMPL) -c $(FLAGS) -I $(BUILD_DIR) -o $@ $(SOURCE_DIR)/$*.mli

$(BUILD_DIR)/%.cmx: $(SOURCE_DIR)/%.ml
	@mkdir -p $(BUILD_DIR)
	$(CMPL) -c $(FLAGS) -I $(BUILD_DIR) -o $@ $(SOURCE_DIR)/$*.ml
