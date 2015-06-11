FLAGS = -g 
##-O6
##-std=c99

## TODO:
ERL_ROOT = /usr/local/lib/erlang/erts-6.4

ECC = erlc

OUTDIR = ./
RCS = $(wildcard *.erl)
OBJS = $(patsubst %.erl,$(OUTDIR)/%.beam,$(RCS))

all:leveldb leveldb_nif.so $(OBJS)

leveldb:
	@cd leveldb-1.15.0 && make

leveldb_nif.so:leveldb_nif.c leveldb-1.15.0/libleveldb.so.1.15
	gcc -o $@ $^ --shared -fpic $(FLAGS) -Wall -I $(ERL_ROOT)/emulator/beam -I $(ERL_ROOT)/include
	
%.beam:%.erl
	$(ECC) $^

clean: 
	rm  leveldb_nif.so *.beam
