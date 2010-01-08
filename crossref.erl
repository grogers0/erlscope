-module(crossref).

-export([build/3]).

-include("crossref.hrl").

-define(CSCOPE_VERSION, 15).

-record(test, {junk, init = "somestring",
        morestuff}).
-record(test1, {stuff, junk}).

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
        element(1, syntax_tree_list_to_crossref(0, Forms))].

syntax_tree_list_to_crossref(LastEndLine, Nodes) ->
    lists:foldl(fun(Node, {Str1, EndLine1}) ->
            {Str2, EndLine2} = syntax_tree_to_crossref(EndLine1, Node),
            {[Str1, Str2], EndLine2} end,
            {[], LastEndLine}, Nodes).

syntax_tree_list_to_crossref(LastEndLine, Separator, Nodes) ->
    syntax_tree_list_to_crossref_acc(Separator, Nodes, {[], LastEndLine}).

syntax_tree_list_to_crossref_acc(_Separator, [], Acc) -> Acc;
syntax_tree_list_to_crossref_acc(_Separator, [Node], {Str1, LastEndLine}) ->
    {Str2, EndLine} = syntax_tree_to_crossref(LastEndLine, Node),
    {[Str1, Str2], EndLine};
syntax_tree_list_to_crossref_acc(Separator, [Node | Rest], {Str1, LastEndLine}) ->
    {Str2, EndLine} = syntax_tree_to_crossref(LastEndLine, Node),
    syntax_tree_list_to_crossref_acc(Separator, Rest,
            {[Str1, Str2, Separator], EndLine}).


%% @spec syntax_tree_to_crossref(integer(), term()) ->
%%          {iolist(), integer()}
%% @doc Outputs a deep list suitable for writing to the crossref file, as well
%% as the last line of the crossref it has written so far
syntax_tree_to_crossref(LastEndLine, Node) ->
    StartLine = erl_syntax:get_pos(Node),
    NodeType = erl_syntax:type(Node),
    {CrossRef, EndLine} = case NodeType of
        attribute -> attribute_to_crossref(StartLine, Node);
        function -> function_to_crossref(StartLine, Node);
        record_field -> record_field(StartLine, Node);
        string -> string(StartLine, Node);
        tuple -> tuple_to_crossref(StartLine, Node);

        error_marker ->
            io:format(standard_error, "parse error: line ~b: ~s~n",
                    [StartLine, erl_syntax:error_marker_info(Node)]),
            {[], StartLine};
        warning_marker ->
            io:format(standard_error, "parse warning: line ~b: ~s~n",
                    [StartLine, erl_syntax:warning_marker_info(Node)]),
            {[], StartLine};
        _ ->
            %io:format(standard_error, "error: line ~b: unhandled syntax tree type: ~s: ~p~n",
                    %[StartLine, NodeType, Node]),
            {[], StartLine}
    end,
    {[
        newline(StartLine, LastEndLine),
        CrossRef
    ], EndLine}.

attribute_to_crossref(LastEndLine, Attribute) ->
    StartLine = erl_syntax:get_pos(Attribute),
    AttributeNameAtom = erl_syntax:atom_value(erl_syntax:attribute_name(Attribute)),
    AttributeArgs = erl_syntax:attribute_arguments(Attribute),
    {CrossRef, EndLine} = case AttributeNameAtom of
        include -> include_to_crossref(StartLine, AttributeArgs);
        record -> record_def_to_crossref(StartLine, AttributeArgs);

        _ -> syntax_tree_list_to_crossref(StartLine, AttributeArgs)
    end,
    {[
        newline(StartLine, LastEndLine),
        io_lib:format("-~s(\n", [AttributeNameAtom]),
        CrossRef,
        ").\n"
    ], EndLine}.

include_to_crossref(LastEndLine, [File]) ->
    StartLine = erl_syntax:get_pos(File),
    {CrossRef, EndLine} = case erl_syntax:type(File) of
        string ->
            {io_lib:format("\t~c\"~s\n\"\n",
                    [?INCLUDE, erl_syntax:string_value(File)]),
                StartLine};
        _ -> syntax_tree_to_crossref(StartLine, File)
    end,
    {[
        newline(StartLine, LastEndLine),
        CrossRef
    ], EndLine};
include_to_crossref(LastEndLine, List) ->
    syntax_tree_list_to_crossref(LastEndLine, List).

record_def_to_crossref(LastEndLine, [Name, Fields]) ->
    StartLine = erl_syntax:get_pos(Name),
    {CrossRef, EndLine} = syntax_tree_to_crossref(StartLine, Fields),
    {[
        newline(StartLine, LastEndLine),
        io_lib:format("\t~c~s\n", [?RECORDDEF, erl_syntax:atom_value(Name)]),
        CrossRef
    ], EndLine}.

atom(LastEndLine, Node) ->
    StartLine = erl_syntax:get_pos(Node),
    {[
        newline(StartLine, LastEndLine),
        io_lib:format("~s\n", [erl_syntax:atom_value(Node)])
    ], StartLine}.

record_field(LastEndLine, Node) ->
    StartLine = erl_syntax:get_pos(Node),
    Name = erl_syntax:record_field_name(Node),
    Value = erl_syntax:record_field_value(Node),
    {CrossRef1, EndLine1} = atom(StartLine, Name),
    {CrossRef2, EndLine} = case Value of
        none -> {[], EndLine1};
        _ -> syntax_tree_to_crossref(EndLine1, Value)
    end,
    {[
        newline(StartLine, LastEndLine),
        CrossRef1,
        if
            Value == none -> [];
            Value /= none -> "=\n"
        end,
        CrossRef2
    ], EndLine}.

string(LastEndLine, Node) ->
    StartLine = erl_syntax:get_pos(Node),
    {[
        newline(StartLine, LastEndLine),
        erl_syntax:string_literal(Node),
        "\n"
    ], StartLine}.

tuple_to_crossref(LastEndLine, Tuple) ->
    StartLine = erl_syntax:get_pos(Tuple),
    {CrossRef, EndLine} = syntax_tree_list_to_crossref(StartLine, ",\n",
        erl_syntax:tuple_elements(Tuple)),
    {[
        newline(StartLine, LastEndLine),
        "{\n",
        CrossRef,
        "}\n"
    ], EndLine}.

function_to_crossref(LastEndLine, FunctionArgs) ->
    StartLine = erl_syntax:get_pos(FunctionArgs),
    FunctionNameAtom = erl_syntax:atom_literal(erl_syntax:function_name(FunctionArgs)),
    {[
        newline(StartLine, LastEndLine),
        io_lib:format("\t~c~s\n", [?FUNCTIONDEFBEGIN, FunctionNameAtom]),
        % @fixme function args, guards, clauses
        io_lib:format("\t~c~s\n", [?FUNCTIONDEFEND, FunctionNameAtom])
    ], StartLine}.



%% utility functions

newline(StartLine, LastEndLine) ->
    if
        StartLine /= LastEndLine ->
            io_lib:format("\n~b \n", [StartLine]);
        true -> []
    end.
