INC=../include
DEST=../bin
NAME=hasmerge.dll

CCMD=ghc -c -O -I$(INC)
HSCMD=ghc -c -O
HSC=hsc2hs -I$(INC)

CC=ghc
CFLAGS=-c -O -I$(INC)
LDFLAGS=-O -shared  -L../lib -lavisynth -package directory

CSOURCES=startend.c

CPPSOURCES=startend_cpp.cpp

HSCSOURCES=HAviSynth/AviSynthFFI.hsc

HLIBSOURCES=$(HSCSOURCES:.hsc=.hs) HAviSynth/AviSynth.hs

HSOURCES=$(HLIBSOURCES) Utils.hs ApplyClip.hs MergeClips.hs Main.hs

SOURCES=$(HSOURCES) $(CSOURCES)
OBJECTS=$(HSOURCES:.hs=.o) $(CSOURCES:.c=.o) $(CPPSOURCES:.cpp=.o)
EXECUTABLE=$(DEST)/$(NAME)


$(EXECUTABLE): $(OBJECTS) 
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@

.c.o:
	$(CC) $(CFLAGS) $< -o $@

.hs.o:
	$(CC) $(CFLAGS) $< -o $@

.hsc.hs:
	$(HSC) $< -o $@

.SUFFIXES: .cpp .c .hs .hsc .o


all: $(SOURCES) $(EXECUTABLE)

clean:
	rm $(OBJECTS) *.hi

