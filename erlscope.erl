-module(erlscope).
-export([main/0]).

main() ->
    Filenames = get_files("cscope.files"),
    crossref:build(["."], ["/usr/include"], Filenames).    

get_files(Filename) ->
    {ok, File} = file:open(Filename, [read, raw, read_ahead]),
    Filenames = read_filenames_from(File, []),
    file:close(File),
    Filenames.

read_filenames_from(File, Acc) ->
    case file:read_line(File) of
        eof -> lists:reverse(Acc);
        {ok, Data} ->
            read_filenames_from(File, [string:strip(Data, both, $\n) | Acc])
    end.
