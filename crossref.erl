-module(crossref).

-export([build/3]).

-define(CSCOPE_VERSION, 15).

%-define(DEFINEBEGIN, $\#).
%-define(DEFINEEND, $\)).
-define(FUNCTIONCALL, $`).
-define(FUNCTIONDEFBEGIN, $\$).
-define(FUNCTIONDEFEND, $}).
%-define(INCLUDE, $~).
-define(NEWFILE, $@).
%-define(RECORDDEF, $s).



build(SrcDirs, IncDirs, SrcFiles) ->
    CscopeDb = "cscope.out",
    TempCscopeDb = "tmp.cscope.out",
    {ok, OutFile} = file:open(TempCscopeDb, [write]),
    Filenames = SrcFiles,  % fix this to use IncDirs to look for files
    TrailerOffset = put_header(OutFile, 0) +
        lists:foldl(fun(Filename, Acc) -> Acc +
                parse_file_write_crossref(Filename, OutFile) end, 0, Filenames),
    put_list(OutFile, SrcDirs, false),
    put_list(OutFile, IncDirs, false),
    put_list(OutFile, SrcFiles, true),
    file:close(OutFile),
    rewrite_header(TempCscopeDb, TrailerOffset),
    file:rename(TempCscopeDb, CscopeDb).


put_header(OutFile, TrailerOffset) ->
    {ok, Cwd} = file:get_cwd(),
    DeepStr = io_lib:format("cscope ~b ~s -c              ~10.10.0b~n",
               [?CSCOPE_VERSION, Cwd, TrailerOffset]),
    file:write(OutFile, DeepStr),
    lists:flatlength(DeepStr).

put_list(OutFile, List, WriteDataSize) ->
    io:format(OutFile, "~b~n", [length(List)]),
    if
        WriteDataSize ->
            DataSize = lists:foldl(fun(Str, Acc) -> length(Str) + Acc + 1 end,
                    0, List),
            io:format(OutFile, "~b~n", [DataSize]);
        true -> ok
    end,
    lists:foreach(fun(Str) -> io:format(OutFile, "~s~n", [Str]) end, List).

rewrite_header(OutFilename, TrailerOffset) ->
    {ok, OutFile} = file:open(OutFilename, [read, write]),
    put_header(OutFile, TrailerOffset),
    file:close(OutFile).
    

parse_file_write_crossref(Filename, OutFile) ->
    {ok, Forms} = epp_dodger:parse_file(Filename),
    %io:format("forms:~n~p~n~n", [Forms]).
    DeepStr = build_crossref_of_file(Filename, Forms),
    file:write(OutFile, DeepStr),
    lists:flatlength(DeepStr).

% @note returns a deep list
build_crossref_of_file(Filename, AbsForms) ->
    [$\t, ?NEWFILE, Filename, $\n, $\n,
        lists:foldl(fun(Form, Acc) -> [Acc, form_to_crossref(Filename, Form)] end,
            [], AbsForms)].


%% Outputs a deep list suitable for writing to the crossref file
form_to_crossref(Filename, Form) ->
    case erl_syntax:type(Form) of
        attribute -> [];  %% @fixme ignore for now
        function -> function_to_crossref(Form);

        error_marker ->
            io:format(standard_error, "parse error: ~s:~b: ~s~n",
                    [Filename, erl_syntax:get_pos(Form), erl_syntax:error_marker_info(Form)]),
            [];
        warning_marker ->
            io:format(standard_error, "parse warning: ~s:~b: ~s~n",
                    [Filename, erl_syntax:get_pos(Form), erl_syntax:warning_marker_info(Form)]),
            [];
        _ ->
            io:format(standard_error, "error: ~s:~b: unhandled form type: ~s~n",
                    [Filename, erl_syntax:get_pos(Form), erl_syntax:type(Form)]),
            []
    end.

function_to_crossref(Function) ->
    StartStr = io_lib:format("~b ~n", [erl_syntax:get_pos(Function)]),
    FunctionName = erl_syntax:atom_name(erl_syntax:function_name(Function)),
    FuncBeginStr = io_lib:format("\t~c~s~n", [?FUNCTIONDEFBEGIN, FunctionName]),
    FuncEndStr = io_lib:format("\t~c~s~n", [?FUNCTIONDEFEND, FunctionName]),
    [StartStr, FuncBeginStr, FuncEndStr, $\n].
    
