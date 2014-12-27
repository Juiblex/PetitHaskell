FLAGS = -use-menhir -cflag -annot
EXEC = petitghc
TARGET = main.native

all:
	ocamlbuild $(FLAGS) $(TARGET)
	rm $(TARGET)
	mv _build/$(TARGET) $(EXEC)

clean:
	ocamlbuild -clean
	rm $(EXEC)
