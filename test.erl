-module(test).
-compile([export_all]).

-define(N, 1000000).

%-define(N, 100).

-record(ets_data, {key, value}).


init()->
     leveldb:load(),
     leveldb:init('/root/leveldb_test.db').
	 

get_string(Length) ->
    lists:foldl(fun(_, Acc) ->
			[random:uniform(255) | Acc]
                end, [], lists:seq(1, Length)).



make_ets_data()->
    ets:new(ets_data, [named_table, set, {keypos, #ets_data.key}, public]),
    insert_ets_data(?N).

insert_ets_data(0)->
    ok;
insert_ets_data(N)->
    Key = get_string(10),
    Value = get_string(100),
    ets:insert(ets_data, #ets_data{
            key = binary:list_to_bin(Key),
            value = binary:list_to_bin(Value)}),
    insert_ets_data(N-1).

insert_to_leveldb()->
    insert_to_leveldb1(ets:first(ets_data), ?N).

insert_to_leveldb1('$end_of_table', _)->
    ok;
insert_to_leveldb1(KeyBin, N)->
    [#ets_data{value = ValueBin}] = ets:lookup(ets_data, KeyBin),
    leveldb:set(KeyBin, ValueBin),
    insert_to_leveldb1(ets:next(ets_data, KeyBin), N-1).


check_data_equal()->
    DbIterator = leveldb:init_iterator(),
    leveldb:iterator_first(DbIterator),
    check_data_equal1(DbIterator).

check_data_equal1(DbIterator)->
    case leveldb:iterator_valid(DbIterator) of
        true->
            {DbKeyBin, DbValuBin} = leveldb:iterator_entry(DbIterator),
            [#ets_data{value = DbValuBin}] = ets:lookup(ets_data, DbKeyBin),
            ets:delete(ets_data, DbKeyBin),
            leveldb:iterator_next(DbIterator),
            check_data_equal1(DbIterator);
        false->
            io:format("~p~n", [ets:tab2list(ets_data)])
        end.
%% test:init().
%% timer:tc(fun()->test:make_ets_data()end). 
%% timer:tc(fun()->test:insert_to_leveldb()end). 
%%  timer:tc(fun()->test:check_data_equal()end). 
%%







