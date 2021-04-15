.POSIX:
.SUFFIXES:

FC      = gfortran
CC      = cc
SRC     = src/oregon.f
TARGET  = oregon

.PHONY: all

all: $(TARGET)

$(TARGET):
	$(FC) $(FFLAGS) $(SRC)
