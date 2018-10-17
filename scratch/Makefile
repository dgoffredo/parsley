FLAGS = $(shell ./flags)
LIBS = -lbdl -lbsl -lpcre2 -lpthread 
SOURCES = main.cpp shibby_lexer.cpp

main: $(SOURCES) flags Makefile
	g++ -I. $(FLAGS) -o main $(SOURCES) $(LIBS)