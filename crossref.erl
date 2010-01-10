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
    ["\t", ?NEWFILE, Filename, "\n",
        element(1, syntax_tree_list(0, Forms))].


%% functions to parse and write the cross reference

atom(LastEndLine, Node) ->
    atom(LastEndLine, Node, [], []).
atom(LastEndLine, Node, PreString, PostString) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            PreString,
            erl_syntax:atom_literal(Node),
            "\n",
            PostString]).

attribute(LastEndLine, Node) ->
    %combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            %"-",
            %erl_syntax:atom_literal(erl_syntax:attribute_name(Node)),
            %"(\n",
            %case Name of
                %include -> include(StartLine, Arguments);
                %record -> record(StartLine, Arguments);
                %define -> define(StartLine, Arguments);
%
                %_ -> syntax_tree_list(StartLine, ",\n", Arguments)
    StartLine = erl_syntax:get_pos(Node),
    Name = erl_syntax:atom_value(erl_syntax:attribute_name(Node)),
    Arguments = erl_syntax:attribute_arguments(Node),
    {CrossRef, EndLine} = case Name of
        include -> include(StartLine, Arguments);
        record -> record(StartLine, Arguments);
        define -> define(StartLine, Arguments);

        _ -> syntax_tree_list(StartLine, ",\n", Arguments)
    end,
    {[
        newline(StartLine, LastEndLine),
        io_lib:format("-~s(\n", [Name]),
        CrossRef,
        ").\n"
    ], EndLine}.

binary(LastEndLine, Node) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            "<<\n",
            {nodelist, ",\n", erl_syntax:binary_fields(Node)},
            ">>\n"]).

binary_field(LastEndLine, Node) ->
    Value = erl_syntax:binary_field_types(Node),
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            {node, erl_syntax:binary_field_body(Node)},
            if
                Value == [] -> [];
                true -> "/\n"
            end,
            if
                Value == [] -> [];
                true -> {nodelist, "-\n", Value}
            end]).

char(LastEndLine, Node) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            erl_syntax:char_literal(Node),
            "\n"]).

define(LastEndLine, [Node1, Node2]) ->
    case erl_syntax:type(Node1) of
        application -> define_func(LastEndLine, [Node1, Node2]);
        atom -> define_const(LastEndLine, [Node1, Node2]);
        variable -> define_const(LastEndLine, [Node1, Node2]);
        _ -> syntax_tree_list(LastEndLine, ",\n", [Node1, Node2])
    end;
define(LastEndLine, Nodes) ->
    syntax_tree_list(LastEndLine, ",\n", Nodes).

define_const(LastEndLine, [Const, Node]) ->
    StartLine = erl_syntax:get_pos(Const),
    {{CrossRefName, EndLineName}, Unknown} = case erl_syntax:type(Const) of
        atom -> {atom(StartLine, Const, ["\t", ?DEFINEBEGIN], []), false};
        variable -> {variable(StartLine, Const, ["\t", ?DEFINEBEGIN], []), false};
        _ -> {syntax_tree(StartLine, Const), true}
    end,
    {CrossRef, EndLine} = syntax_tree(EndLineName, Node),
    {[
        newline(StartLine, LastEndLine),
        CrossRefName,
        ",\n",
        CrossRef,
        if
            not Unknown -> io_lib:format("\t~c\n", [?DEFINEEND]);
            true -> []
        end
    ], EndLine}.

define_func(LastEndLine, [Func, Node]) ->
    StartLine = erl_syntax:get_pos(Func),
    Oper = erl_syntax:application_operator(Func),
    {{CrossRefFunc, EndLineFunc}, Unknown} = case erl_syntax:type(Oper) of
        atom -> {atom(StartLine, Oper, ["\t", ?DEFINEBEGIN], []), false};
        variable -> {variable(StartLine, Oper, ["\t", ?DEFINEBEGIN], []), false};
        _ -> {syntax_tree(StartLine, Oper), true}
    end,
    {CrossRef, EndLine} = syntax_tree(EndLineFunc, Node),
    {[
        newline(StartLine, LastEndLine),
        CrossRefFunc,
        ",\n",
        CrossRef,
        if
            not Unknown -> io_lib:format("\t~c\n", [?DEFINEEND]);
            true -> []
        end
    ], EndLine}.

float(LastEndLine, Node) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            erl_syntax:float_literal(Node),
            "\n"]).

function(LastEndLine, Node) ->
    NameStr = erl_syntax:atom_literal(erl_syntax:function_name(Node)),
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            io_lib:format("\t~c~s\n",  [?FUNCTIONDEFBEGIN, NameStr]),
            % @fixme - function args, guards, clauses
            io_lib:format("\t~c~s\n",  [?FUNCTIONDEFEND, NameStr])]).

include(LastEndLine, [File]) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(File)},
            case erl_syntax:type(File) of
                string -> io_lib:format("\t~c\"~s\n\"\n",
                    [?INCLUDE, erl_syntax:string_value(File)]);
                _ -> {node, File}
            end]);
include(LastEndLine, Nodes) ->
    syntax_tree_list(LastEndLine, Nodes).

integer(LastEndLine, Node) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            erl_syntax:integer_literal(Node),
            "\n"]).

nil(LastEndLine, Node) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)}, "[]\n"]).

record(LastEndLine, [Name, Fields]) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Name)},
            io_lib:format("\t~c~s\n", [?RECORDDEF, erl_syntax:atom_value(Name)]),
            {node, Fields}]);
record(LastEndLine, Nodes) ->
    syntax_tree_list(LastEndLine, Nodes).

record_field(LastEndLine, Node) ->
    Value = erl_syntax:record_field_value(Node),
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            {node, erl_syntax:record_field_name(Node)},
            if
                Value == none -> [];
                true -> "=\n"
            end,
            if
                Value == none -> [];
                true -> {node, Value}
            end]).

%% @note The line number position of this Node is wrong - it is always 0
size_qualifier(LastEndLine, Node) ->
    combine(LastEndLine, [{node, erl_syntax:size_qualifier_body(Node)},
            ":\n",
            {node, erl_syntax:size_qualifier_argument(Node)}]).

string(LastEndLine, Node) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            "\"",
            erl_syntax:string_value(Node),
            "\n\"\n"]).

%% @spec syntax_tree(integer(), term()) ->
%%          {iolist(), integer()}
%% @doc Outputs a deep list suitable for writing to the crossref file, as well
%% as the last line of the crossref it has written so far
syntax_tree(LastEndLine, Node) ->
    NodeType = erl_syntax:type(Node),
    case NodeType of
        atom -> atom(LastEndLine, Node);
        attribute -> attribute(LastEndLine, Node);
        binary -> binary(LastEndLine, Node);
        binary_field -> binary_field(LastEndLine, Node);
        char -> char(LastEndLine, Node);
        float -> float(LastEndLine, Node);
        function -> function(LastEndLine, Node);
        integer -> integer(LastEndLine, Node);
        nil -> nil(LastEndLine, Node);
        record_field -> record_field(LastEndLine, Node);
        size_qualifier -> size_qualifier(LastEndLine, Node);
        string -> string(LastEndLine, Node);
        tuple -> tuple(LastEndLine, Node);
        variable -> variable(LastEndLine, Node);

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
            {Str2, EndLine2} = syntax_tree(EndLine1, Node),
            {[Str2 | Str1], EndLine2} end,
            {[], LastEndLine}, Nodes),
    {lists:reverse(Str), EndLine}.

syntax_tree_list(LastEndLine, Separator, Nodes) ->
    syntax_tree_list_acc(Separator, Nodes, {[], LastEndLine}).

syntax_tree_list_acc(_Separator, [], {Str, LastEndLine}) ->
    {lists:reverse(Str), LastEndLine};
syntax_tree_list_acc(Separator, [Node], {Str1, LastEndLine}) ->
    {Str2, EndLine} = syntax_tree(LastEndLine, Node),
    syntax_tree_list_acc(Separator, [], {[Str2 | Str1], EndLine});
syntax_tree_list_acc(Separator, [Node | Rest], {Str1, LastEndLine}) ->
    {Str2, EndLine} = syntax_tree(LastEndLine, Node),
    syntax_tree_list_acc(Separator, Rest, {[Separator, Str2 | Str1], EndLine}).

tuple(LastEndLine, Node) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            "{\n",
            {nodelist, ",\n", erl_syntax:tuple_elements(Node)},
            "}\n"]).

variable(LastEndLine, Node) ->
    variable(LastEndLine, Node, [], []).
variable(LastEndLine, Node, PreString, PostString) ->
    combine(LastEndLine, [{newline, erl_syntax:get_pos(Node)},
            PreString,
            erl_syntax:variable_literal(Node),
            "\n",
            PostString]).


%% utility functions

combine(LastEndLine, List) ->
    combine(LastEndLine, List, []).

combine(LastEndLine, [], Acc) ->
    {lists:reverse(Acc), LastEndLine};
combine(LastEndLine, [{node, Node} | Rest], Acc) ->
    {CrossRef, EndLine} = syntax_tree(LastEndLine, Node),
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
combine(LastEndLine, [String | Rest], Acc) when is_list(String) ->
    combine(LastEndLine, Rest, [String | Acc]).


newline(StartLine, LastEndLine) ->
    if
        StartLine /= LastEndLine ->
            io_lib:format("\n~b \n", [StartLine]);
        true -> []
    end.
