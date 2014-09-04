-module(leveldb).
-export([load/0, init/1, set/2, get/1, del/1, init_iterator/0, iterator_first/1, iterator_last/1, iterator_valid/1, iterator_prev/1, iterator_next/1, iterator_entry/1]).

load()->
	leveldb_nif:load().

init(LeveldbDbFile) when is_atom(LeveldbDbFile)->
	leveldb_nif:init(LeveldbDbFile).

set(KeyBin, ValueBin) when is_binary(KeyBin), is_binary(ValueBin)->
    leveldb_nif:set(KeyBin, ValueBin).

get(KeyBin) when is_binary(KeyBin)->
    leveldb_nif:get(KeyBin).


del(KeyBin) when is_binary(KeyBin)->
    leveldb_nif:del(KeyBin).

init_iterator()->
    leveldb_nif:init_iterator().

iterator_first(Iterator)->
    leveldb_nif:iterator_first(Iterator).

iterator_last(Iterator)->
    leveldb_nif:iterator_last(Iterator).


iterator_valid(Iterator)->
    leveldb_nif:iterator_valid(Iterator).

iterator_prev(Iterator)->
    leveldb_nif:iterator_prev(Iterator).


iterator_next(Iterator)->
    leveldb_nif:iterator_next(Iterator).

iterator_entry(Iterator)->
    leveldb_nif:iterator_entry(Iterator).

