-module(crossref).

-export([build/3]).

-include("crossref.hrl").

-define(CSCOPE_VERSION, 15).

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
        attribute -> attribute_to_crossref(Form);
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

attribute_to_crossref(Attribute) ->
    case erl_syntax:atom_value(erl_syntax:attribute_name(Attribute)) of
        include ->
            StartStr = io_lib:format("~b -include(~n", [erl_syntax:get_pos(Attribute)]),
            [File] = erl_syntax:attribute_arguments(Attribute),
            IncludeStr = io_lib:format("\t~c\"~s~n", [?INCLUDE, erl_syntax:string_value(File)]),
            EndStr = "\").\n",
            [StartStr, IncludeStr, EndStr];

        _ -> []  % ignore unused attributes
    end.

function_to_crossref(Function) ->
    StartStr = io_lib:format("~b ~n", [erl_syntax:get_pos(Function)]),
    FunctionName = erl_syntax:atom_name(erl_syntax:function_name(Function)),
    FuncBeginStr = io_lib:format("\t~c~s~n", [?FUNCTIONDEFBEGIN, FunctionName]),
    FuncEndStr = io_lib:format("\t~c~s~n", [?FUNCTIONDEFEND, FunctionName]),
    [StartStr, FuncBeginStr, FuncEndStr, $\n].
    
