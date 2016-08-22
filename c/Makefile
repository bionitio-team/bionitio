
CC=gcc
CFLAGS=-Wall
DEBUG=-g
LIBS=-lm -lz
OPT=-O3

# compiler flags:
#  -g    adds debugging information to the executable file
#  -Wall turns on most, but not all, compiler warnings

# mac
CFLAGS=-Wall -m64

OBJ=obj/biotool.o obj/fasta.o obj/log.o
TARGET=biotool-c

default: clean obj $(TARGET)

clean:
	-rm obj/*.o ./biotool* 

$(TARGET): obj 
	$(CC) $(CFLAGS) $(OPT) $(LDFLAGS) -o $(TARGET) $(OBJ) $(LIBS)

objdir:
	mkdir -p obj

obj: clean objdir $(OBJ)

obj/%.o: src/%.c
	$(CC) $(CFLAGS) $(OPT) -c $? -o $@ 

