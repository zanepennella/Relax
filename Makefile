# Compiler and flags
FC = gfortran
FFLAGS = -Wall -O -g -ffixed-line-length-100

# Target executable
TARGET = relax

# Source files
SRCS = relax.f choices.f setup.f jacobi.f gauss_seidel.f

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

# Clean up build files
clean:
	rm -f $(OBJS) $(TARGET)

# Phony targets (not actual files)
.PHONY: all clean