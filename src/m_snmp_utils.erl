-module(m_snmp_utils).
-export([walk/3, pretty_varbind/1]).

-include_lib("snmp/include/snmp_types.hrl").

walk(UserId, TargetName, Oid) ->
    walk_next(UserId, TargetName, Oid, Oid, []).


walk_next(UserId, TargetName, FirstOid, Oid, Acc) ->
    {ok, {noError, 0, VarBinds}, _Remaining} =
        snmpm:sync_get_next(UserId, TargetName, [Oid]),
    {NewOid, NewAcc} = 
        traverse_var_binds(VarBinds, Acc),
    
    IsDesc = is_descendant(FirstOid, NewOid),
    if
        IsDesc, NewOid =/= Oid ->
            walk_next(UserId, TargetName, FirstOid, NewOid, NewAcc);
        true ->
            lists:reverse(Acc)
    end.


pretty_varbind(#varbind{oid=Oid, value=Value}) ->
    NewOid = pretty_oid(Oid),
    {NewOid, Value}.


pretty_oid(Oid) ->
    pretty_oid(Oid, []).


pretty_oid([], Skip) -> lists:reverse(Skip);
pretty_oid(Oid, Skip) ->
    case snmpm:oid_to_name(Oid) of
        {ok, Name} -> [Name] ++ lists:reverse(Skip);
        {error, _} -> 
            [Last|RevHead] = lists:reverse(Oid),
            NewOid = lists:reverse(RevHead),
            pretty_oid(NewOid, [Last|Skip])
    end.


is_descendant([H|T1], [H|T2]) ->
    is_descendant(T1, T2);
is_descendant([], [_|_]) ->
    true;
is_descendant(_, _) ->
    false.


%% Adds VarBinds to Acc in reversed order;
%% Returns {LastVarBind, NewAcc}.
traverse_var_binds([H], Acc) -> 
    {H#varbind.oid, [H|Acc]};
traverse_var_binds([H|T], Acc) -> 
    traverse_var_binds(T, [H|Acc]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_descendant_test_() ->
    [?_assertEqual(is_descendant([1,2,3], [1,2,3,4]), true)
    ,?_assertEqual(is_descendant([1,2,3], [1,2,3]), false)
    ,?_assertEqual(is_descendant([1,2,3], [1,2]), false)
    ].

-endif.
