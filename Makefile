# Compiler and flags
FC = gfortran
FFLAGS = -Wall -O -g -ffixed-line-length-100

# Target executable
TARGET = relax

# Source files
SRCS = relax.f choices.f setup.f jacobi.f gauss_seidel.f write_charge_density.f

# Object files (generated from source files)
OBJS = $(SRCS:.f=.o)

# Default target
all: $(TARGET)

# Rule to build the executable
$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS)

# Rule to compile .f files into .o files
%.o: %.f
	$(FC) $(FFLAGS) -c $< -o $@


# Clean up build files and generated outputs
clean:
	@echo "Cleaning up..."
	rm -f $(OBJS) $(TARGET) potential_movie.mp4
	rm -f *.o *.O *.mod *.MOD relax *.dat *.png *.mp4 *.txt *.10

# Phony targets (not actual files)
.PHONY: all movie clean