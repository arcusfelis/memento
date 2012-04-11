-module(m_mib_compiler).
-export([mib_to_meta/1]).


mib_to_meta(FileName) ->
    %% Generate data into a temp directory
    TempDir = mochitemp:mkdtemp(),
    Meta =
        try
            compile_mib_to_bin(FileName, TempDir)
        after
            %% Clean temp data
            mochitemp:rmtempdir(TempDir)
        end,
    Meta.


compile_mib_to_bin(FileName, TempDir) ->
    snmpc:compile(FileName, [description, {outdir, TempDir}]),
    %% Name without `.mib'
    Name = filename:basename(FileName),
    %% Form the name of the binary file
    BinFile = filename:join(TempDir, Name ++ ".bin"),
    io:write(BinFile),
    snmpc_misc:read_mib(BinFile).
