#REFERENCE
#http://www.cs.swarthmore.edu/~newhall/unixhelp/howto_makefiles.html

# define the C compiler to use
CC = g++

# define any compile-time flags
CFLAGS = -Wall -g

# define any directories containing header files other than /usr/include
INCLUDES = -I/usr/local/include -I/usr/local/include/sphinxbase -I/usr/local/include/pocketsphinx 

# define library paths in addition to /usr/lib
#   if I wanted to include libraries not in /usr/lib I'd specify
#   their path using -Lpath, something like:
LFLAGS= -L/usr/local/lib

# define any libraries to link into executable:
#   if I want to link in libraries (libx.so or libx.a) I use the -llibname 
#   option, something like (this will link in libmylib.so and libm.so:
LIBS= -lpocketsphinx -lsphinxbase -lsphinxad 

# define the CPP source files
SRCS = main.cpp

# define the executable file 
MAIN = main

all: main.cpp
	$(CC) $(CFLAGS) $(SRCS) $(INCLUDES) $(LFLAGS) $(LIBS) -o $(MAIN)
