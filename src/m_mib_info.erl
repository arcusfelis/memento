-module(m_mib_info).
-compile([export_all]).

-include_lib("snmp/include/snmp_types.hrl").


read_binary_mib_file(FileName) ->
    {ok, Mib} = snmpc_misc:read_mib(FileName),
    Mib.

objects(#mib{mes = Mes}) ->
    [X#me.oid || X <- Mes].


variables(#mib{mes = Mes}) ->
    [X#me.oid || X <- Mes, X#me.entrytype =:= variable].


test() ->
    FileName = code:priv_dir(snmp) ++ "/mibs/STANDARD-MIB.bin",
    Mib = read_binary_mib_file(FileName),
    variables(Mib).
