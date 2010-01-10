-module(crossref).

-export([build/3]).

-include("crossref.hrl").

-define(CSCOPE_VERSION, 15).

-define(tEST_FLOAT, 140.25).
-define(TEST_ATOM, test_some_atom).
-define(TEST_STRING, "test some string").
-define(TEST_NIL, []).
-define(TEST_LIST, [12, 15, 26]).
-define(TEST_TUPLE, {12, 15, 26}).
-define(TEST_BINARY1, <<12, 15:8, 26/little-signed, 52:32/native-unsigned>>).
-define(TEST_BINARY2, <<>>).

-define(TEST_FUN(X, Y, Z), [{x, X}, {y, Y}, {z, Z}]).

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
    ["\n\t", ?NEWFILE, Filename, "\n",
        element(1, syntax_tree_list(0, Forms))].


%%% functions to parse and write the cross reference

atom(LastEndLine, Node, PreStr) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            PreStr,
            erl_syntax:atom_literal(Node),
            "\n"]).

application(LastEndLine, Node, PreStr) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            {node, erl_syntax:application_operator(Node), PreStr},
            "(\n",
            {nodelist, ",\n", erl_syntax:application_arguments(Node)},
            ")\n"]).

attribute(LastEndLine, Node, _PreStr) ->
    Name = erl_syntax:attribute_name(Node),
    Args = erl_syntax:attribute_arguments(Node),
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Name)},
            "-",
            {node, Name},
            "(\n",
            case erl_syntax:atom_value(Name) of
                include -> include_combine(Args);
                record -> record_combine(Args);
                define -> define_combine(Args);

                _ -> {nodelist, ",\n", Args}
            end,
            ").\n"]).

binary(LastEndLine, Node, _PreStr) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            "<<\n",
            {nodelist, ",\n", erl_syntax:binary_fields(Node)},
            ">>\n"]).

binary_field(LastEndLine, Node, _PreStr) ->
    Value = erl_syntax:binary_field_types(Node),
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            {node, erl_syntax:binary_field_body(Node)},
            if
                Value == [] -> [];
                true -> {combine, ["/\n", {nodelist, "-\n", Value}]}
            end]).

char(LastEndLine, Node, PreStr) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            PreStr,
            erl_syntax:char_literal(Node),
            "\n"]).

define_combine([Node1, Node2]) ->
    {combine, [{newline, erl_syntax:get_pos(Node1)},
        {node, Node1, ["\t", ?DEFINEBEGIN]},
        ",\n",
        {node, Node2},
        "\t",
        ?DEFINEEND]}.

float(LastEndLine, Node, PreStr) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            PreStr,
            erl_syntax:float_literal(Node),
            "\n"]).

function(LastEndLine, Node, _PreStr) ->
    Name = erl_syntax:function_name(Node),
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            {node, Name, ["\t", ?FUNCTIONDEFBEGIN]},
            % @fixme - function args, guards, clauses
            {node, Name, ["\t", ?FUNCTIONDEFEND]}]).

include_combine([File]) ->
    {combine, [{newline, erl_syntax:get_pos(File)},
            {node, File, ["\t", ?INCLUDE]}]};
include_combine(Nodes) ->
    {nodelist, ",\n", Nodes}.

integer(LastEndLine, Node, PreStr) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            PreStr,
            erl_syntax:integer_literal(Node),
            "\n"]).

nil(LastEndLine, Node, PreStr) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            PreStr,
            "[]\n"]).

record_combine([Name, Fields]) ->
    {combine, [{newline, erl_syntax:get_pos(Name)},
            {node, Name, ["\t", ?RECORDDEF]},
            {node, Fields}]};
record_combine(Nodes) ->
    {nodelist, ",\n", Nodes}.

record_field(LastEndLine, Node, _PreStr) ->
    Value = erl_syntax:record_field_value(Node),
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            {node, erl_syntax:record_field_name(Node)},
            if
                Value == none -> [];
                true -> {combine, ["=\n", {node, Value}]}
            end]).

%% @note The line number position of this Node is wrong - it is always 0
size_qualifier(LastEndLine, Node, PreStr) ->
    combine(LastEndLine, [PreStr,
            {node, erl_syntax:size_qualifier_body(Node)},
            ":\n",
            {node, erl_syntax:size_qualifier_argument(Node)}]).

string(LastEndLine, Node, PreStr) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            PreStr,
            "\"",
            erl_syntax:string_value(Node),
            "\n\"\n"]).

%% @spec syntax_tree(integer(), term(), string(), string()) ->
%%          {iolist(), integer()}
%% @doc Outputs a deep list suitable for writing to the crossref file, as well
%% as the last line of the crossref it has written so far
syntax_tree(LastEndLine, Node, PreStr) ->
    NodeType = erl_syntax:type(Node),
    case NodeType of
        atom -> atom(LastEndLine, Node, PreStr);
        application -> application(LastEndLine, Node, PreStr);
        attribute -> attribute(LastEndLine, Node, PreStr);
        binary -> binary(LastEndLine, Node, PreStr);
        binary_field -> binary_field(LastEndLine, Node, PreStr);
        char -> char(LastEndLine, Node, PreStr);
        float -> float(LastEndLine, Node, PreStr);
        function -> function(LastEndLine, Node, PreStr);
        integer -> integer(LastEndLine, Node, PreStr);
        nil -> nil(LastEndLine, Node, PreStr);
        record_field -> record_field(LastEndLine, Node, PreStr);
        size_qualifier -> size_qualifier(LastEndLine, Node, PreStr);
        string -> string(LastEndLine, Node, PreStr);
        tuple -> tuple(LastEndLine, Node, PreStr);
        variable -> variable(LastEndLine, Node, PreStr);

        error_marker ->
            io:format(standard_error, "parse error: ~s~n",
                    [erl_syntax:error_marker_info(Node)]),
            {[], LastEndLine};
        warning_marker ->
            io:format(standard_error, "parse warning: ~s~n",
                    [erl_syntax:warning_marker_info(Node)]),
            {[], LastEndLine};
        _ ->
            io:format(standard_error, "error: unhandled syntax tree type: ~s: ~p~n",
                    [NodeType, Node]),
            {[], LastEndLine}
    end.

syntax_tree_list(LastEndLine, Nodes) ->
    {Str, EndLine} = lists:foldl(fun(Node, {Str1, EndLine1}) ->
            {Str2, EndLine2} = syntax_tree(EndLine1, Node, []),
            {[Str2 | Str1], EndLine2} end,
            {[], LastEndLine}, Nodes),
    {lists:reverse(Str), EndLine}.

syntax_tree_list(LastEndLine, Separator, Nodes) ->
    syntax_tree_list_acc(Separator, Nodes, {[], LastEndLine}).

syntax_tree_list_acc(_Separator, [], {Str, LastEndLine}) ->
    {lists:reverse(Str), LastEndLine};
syntax_tree_list_acc(Separator, [Node], {Str1, LastEndLine}) ->
    {Str2, EndLine} = syntax_tree(LastEndLine, Node, []),
    syntax_tree_list_acc(Separator, [], {[Str2 | Str1], EndLine});
syntax_tree_list_acc(Separator, [Node | Rest], {Str1, LastEndLine}) ->
    {Str2, EndLine} = syntax_tree(LastEndLine, Node, []),
    syntax_tree_list_acc(Separator, Rest, {[Separator, Str2 | Str1], EndLine}).

tuple(LastEndLine, Node, _PreStr) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            "{\n",
            {nodelist, ",\n", erl_syntax:tuple_elements(Node)},
            "}\n"]).

variable(LastEndLine, Node, PreStr) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            PreStr,
            erl_syntax:variable_literal(Node),
            "\n"]).


%%% utility functions

combine(LastEndLine, List) ->
    combine(LastEndLine, List, []).

combine(LastEndLine, [], Acc) ->
    {lists:reverse(Acc), LastEndLine};
combine(LastEndLine, [{node, Node} | Rest], Acc) ->
    {CrossRef, EndLine} = syntax_tree(LastEndLine, Node, []),
    combine(EndLine, Rest, [CrossRef | Acc]);
combine(LastEndLine, [{node, Node, PreStr} | Rest], Acc) ->
    {CrossRef, EndLine} = syntax_tree(LastEndLine, Node, PreStr),
    combine(EndLine, Rest, [CrossRef | Acc]);
combine(LastEndLine, [{nodelist, Nodes} | Rest], Acc) ->
    {CrossRef, EndLine} = syntax_tree_list(LastEndLine, Nodes),
    combine(EndLine, Rest, [CrossRef | Acc]);
combine(LastEndLine, [{nodelist, Sep, Nodes} | Rest], Acc) ->
    {CrossRef, EndLine} = syntax_tree_list(LastEndLine, Sep, Nodes),
    combine(EndLine, Rest, [CrossRef | Acc]);
combine(LastEndLine, [{newline, Line} | Rest], Acc) ->
    CrossRef = newline(Line, LastEndLine),
    combine(Line, Rest, [CrossRef | Acc]);
combine(LastEndLine, [{combine, List} | Rest], Acc) ->
    combine(LastEndLine, List ++ Rest, Acc);
combine(LastEndLine, [String | Rest], Acc) ->
    combine(LastEndLine, Rest, [String | Acc]).


newline(StartLine, LastEndLine) ->
    if
        StartLine /= LastEndLine ->
            io_lib:format("\n~b \n", [StartLine]);
        true -> []
    end.
