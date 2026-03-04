# Makefile
FC = gfortran
FFLAGS = -O2 -Wall
SRCDIR = src
TARGET = main

all:
	$(FC) $(FFLAGS) $(SRCDIR)/main.f90 -o $(TARGET)

clean:
	rm -f *.o *.mod $(TARGET)

rebuild: clean all

.PHONY: all clean rebuild
