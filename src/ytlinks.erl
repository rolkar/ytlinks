-module(ytlinks).

-export([test_traverse_add/0,
	 test_traverse_sum/0,
	 test_file/0,
	 test_file2/0]).

-export([url_test_file2/0]).

test_traverse_add() ->
    traverse(base_dir(), fun add/2, []).

test_traverse_sum() ->
    traverse(base_dir(), fun sum/2, 0).

test_file() ->
    {ok, Url} = parse_url_file(url_test_file()),
    {ok, {{_,200,"OK"}, _, Body}} = httpc:request(Url),
    {ok, {Channel, ChannelOwner}} = find_channel(Body),
    {Url, Channel, ChannelOwner}.

test_file2() ->
    {ok, Url} = parse_url_file(url_test_file2()),
    {ok, {{_,200,"OK"}, _, Body}} = httpc:request(Url),
    {ok, {Channel, ChannelOwner}} = find_channel(Body),
    {Url, Channel, ChannelOwner}.

traverse(Dir, Fun, Acc) ->
    filelib:fold_files(Dir, ".*url", true, Fun, Acc).

parse_url_file(File) ->
    {ok, Content} = file:read_file(File),
    Lines = binary:split(Content, [<<"\n">>, <<"\r">>], [global, trim_all]),
    find_url(Lines).

add(Filename, Acc) ->
    [Filename | Acc].

sum(_Filename, Acc) ->
    Acc + 1.

find_url([]) ->
    {error, no_url};
find_url([Line|Lines]) ->
    case Line of
	<<"URL=", Url/binary>> ->
	    {ok, Url};
	_ ->
	    find_url(Lines)
    end.

find_channel(Body) ->
    StartPattern = "\"channel\":{\"simpleText\":\"",
    PL = length(StartPattern),
    case string:str(Body, StartPattern) of
	0 ->
	    {error, no_channel};
	N ->
	    Rest = lists:sublist(Body, N+PL, 1000),
	    EndPattern = "\"}",
	    case string:str(Rest, EndPattern) of
		0 ->
		    {error, no_end_of_channel};
		M ->
		    Channel = lists:sublist(Rest, 1, M-1),
		    {ok, ChannelOwner} = find_channel_owner(Rest),
		    {ok, {Channel, ChannelOwner}}
	    end
    end.

find_channel_owner(Body) ->
    StartPattern = "\"commandMetadata\":{\"webCommandMetadata\":{\"url\":\"/",
    PL = length(StartPattern),
    case string:str(Body, StartPattern) of
	0 ->
	    {error, no_channel_owner};
	N ->
	    Rest = lists:sublist(Body, N+PL, 100),
	    EndPattern = "\",",
	    case string:str(Rest, EndPattern) of
		0 ->
		    {error, no_end_of_channel_owner};
		M ->
		    ChannelOwner = lists:sublist(Rest, 1, M-1),
		    {ok, ChannelOwner}
	    end
    end.

base_dir() ->
    "/mnt/c/Users/roland/Desktop/musik/".

url_test_file() ->
    filename:join([base_dir(),
		   "Lovebites",
		   "reactions/Thunder Vengeance",
		   "What does SNAKE SABO from SKID ROW think about LOVEBITES-"
		   " - YouTube.url"]).

url_test_file2() ->
    filename:join([base_dir(),
		   "BAND-MAID",
		   "reactions/Domination/Live",
		   "First Time Hearing Band-Maid 🎵 Domination Reaction"
		   " - YouTube.url"]).
