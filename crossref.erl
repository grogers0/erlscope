-module(crossref).

-export([build/3]).

-include("crossref.hrl").

-define(CSCOPE_VERSION, 15).

-record(test, {junk, init = "somestring",
        morestuff}).

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
build_crossref_of_file(Filename, Forms) ->
    ["\t", ?NEWFILE, Filename, "\n",
        element(1, fold_syntax_tree_list_to_crossref(0, Forms))].

fold_syntax_tree_list_to_crossref(LastEndLine, Forms) ->
    lists:foldl(fun(Form, {Str1, EndLine1}) ->
            {Str2, EndLine2} = syntax_tree_to_crossref(EndLine1, Form),
            {[Str1, Str2], EndLine2} end,
            {[], LastEndLine}, Forms).

%% @spec syntax_tree_to_crossref(integer(), term()) ->
%%          {iolist(), integer()}
%% @doc Outputs a deep list suitable for writing to the crossref file, as well
%% as the last line of the crossref it has written so far
syntax_tree_to_crossref(LastEndLine, Tree) ->
    StartLine = erl_syntax:get_pos(Tree),
    TreeType = erl_syntax:type(Tree),
    {CrossRef, EndLine} = case TreeType of
        attribute -> attribute_to_crossref(StartLine, Tree);
        function -> function_to_crossref(StartLine, Tree);

        error_marker ->
            io:format(standard_error, "parse error: line ~b: ~s~n",
                    [StartLine, erl_syntax:error_marker_info(Tree)]),
            {[], StartLine};
        warning_marker ->
            io:format(standard_error, "parse warning: line ~b: ~s~n",
                    [StartLine, erl_syntax:warning_marker_info(Tree)]),
            {[], StartLine};
        _ ->
            io:format(standard_error, "error: line ~b: unhandled syntax tree type: ~s: ~p~n",
                    [StartLine, TreeType, Tree]),
            {[], StartLine}
    end,
    {[
        if
            StartLine /= LastEndLine -> io_lib:format("\n~b \n", [StartLine]);
            true -> []
        end,
        CrossRef
    ], EndLine}.

attribute_to_crossref(LastEndLine, Attribute) ->
    StartLine = erl_syntax:get_pos(Attribute),
    AttributeNameAtom = erl_syntax:atom_value(erl_syntax:attribute_name(Attribute)),
    AttributeArgs = erl_syntax:attribute_arguments(Attribute),
    {CrossRef, EndLine} = case AttributeNameAtom of
        include -> include_to_crossref(StartLine, AttributeArgs);
        record -> record_to_crossref(StartLine, AttributeArgs);

        _ -> fold_syntax_tree_list_to_crossref(StartLine, AttributeArgs)
    end,
    {[
        if
            StartLine /= LastEndLine ->
                io_lib:format("\n~b ", [StartLine]);
            true -> []
        end,
        io_lib:format("-~s(\n", [AttributeNameAtom]),
        CrossRef,
        ").\n"
    ], EndLine}.

include_to_crossref(LastEndLine, IncludeArgs) ->
    [File] = IncludeArgs,
    StartLine = erl_syntax:get_pos(File),
    {[
        if
            StartLine /= LastEndLine ->
                io_lib:format("\n~b ", [StartLine]);
            true -> []
        end,
        io_lib:format("\t~c\"~s\n", [?INCLUDE, erl_syntax:string_value(File)]),
        "\"\n"
    ], StartLine}.

record_to_crossref(LastEndLine, RecordArgs) ->
    StartLine = LastEndLine,
    {[
        if
            StartLine /= LastEndLine ->
                io_lib:format("\n~b \n", [StartLine]);
            true -> []
        end,
        [] % @fixme record fields tuple
    ], StartLine}.

function_to_crossref(LastEndLine, FunctionArgs) ->
    StartLine = erl_syntax:get_pos(FunctionArgs),
    FunctionNameAtom = erl_syntax:atom_literal(erl_syntax:function_name(FunctionArgs)),
    {[
        if
            StartLine /= LastEndLine ->
                io_lib:format("\n~b \n", [StartLine]);
            true -> []
        end,
        io_lib:format("\t~c~s\n", [?FUNCTIONDEFBEGIN, FunctionNameAtom]),
        % @fixme function args, guards, clauses
        io_lib:format("\t~c~s\n", [?FUNCTIONDEFEND, FunctionNameAtom])
    ], StartLine}.

