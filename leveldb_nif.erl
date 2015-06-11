-module(leveldb_nif).
-export([load/0, init/1, set/2, get/1, del/1, init_iterator/1, iterator_first/1, iterator_last/1, iterator_valid/1, iterator_prev/1, iterator_next/1, iterator_entry/1]).

-export([create_snapshot/0, release_snapshot/0]).

load()->
	erlang:load_nif("./leveldb_nif", 0).

init(_)->
	"leveldb_nif library not loaded".

set(_,_)->
	"leveldb_nif library not loaded".

get(_)->
	"leveldb_nif library not loaded".

del(_)->
	"leveldb_nif library not loaded".

init_iterator(_)->
	"leveldb_nif library not loaded".

iterator_first(_)->
	"leveldb_nif library not loaded".

iterator_last(_)->
	"leveldb_nif library not loaded".


iterator_valid(_)->
	"leveldb_nif library not loaded".

iterator_prev(_)->
	"leveldb_nif library not loaded".


iterator_next(_)->
	"leveldb_nif library not loaded".

iterator_entry(_)->
	"leveldb_nif library not loaded".

create_snapshot()->
	"leveldb_nif library not loaded".

release_snapshot()->
	"leveldb_nif library not loaded".





