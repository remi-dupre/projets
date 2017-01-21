CXX=g++ # Compilateur
EXEC=satsol # Les executables à générer

FLAGS=-std=c++1y -O3 -s -Wall

BUILD_DIR=build
SOURCE_DIR=./
SRC=$(wildcard $(SOURCE_DIR)/*.cpp) # La liste des .cpp
OBJ=$(patsubst $(SOURCE_DIR)/%.cpp,$(BUILD_DIR)/%.o,$(SRC))

all: $(EXEC)

# Nettoye les dossiers de compilation
clean:
	rm -rf $(BUILD_DIR)/
	rm $(EXEC)

# Création de l'executable
satsol: $(OBJ)
	$(CXX) -o $@ $^ $(FLAGS)

# Compilation des fichiers
$(BUILD_DIR)/%.o: $(SOURCE_DIR)/%.cpp
	@mkdir -p $(BUILD_DIR)
	$(CXX) -c -o $@ $< $(FLAGS) $(FILTERS)
