
CC=gcc
CFLAGS=-Wall
DEBUG=-g
LIBS=-lm -lz
OPT=-O3

# mac
CFLAGS=-Wall -m64 -std=c99

INSTALL_DIR=/usr/local/bin
OBJ=obj/biotool.o obj/fasta.o obj/log.o
TARGET=biotool-c

default: obj $(TARGET)

clean:
	-rm obj/*.o ./biotool* 

install: $(TARGET)
	install -m 0755 $(TARGET) $(INSTALL_DIR)
.PHONY: install

$(TARGET): obj 
	$(CC) $(CFLAGS) $(OPT) $(LDFLAGS) -o $(TARGET) $(OBJ) $(LIBS)

objdir:
	mkdir -p obj

obj: objdir $(OBJ)

obj/%.o: src/%.c
	$(CC) $(CFLAGS) $(OPT) -c $? -o $@ 

