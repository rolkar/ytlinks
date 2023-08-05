-module(ytlinks).

-export([analyze/0, analyze/1]).

-export([test_traverse_add/0,
	 test_traverse_sum/0,
	 test_file/0,
	 test_file2/0]).

-export([url_test_file2/0]).

%% API

analyze() ->
    analyze(base_dir()).

analyze(BaseDir) ->
    BaseLen = length(BaseDir) + 1,
    traverse(base_dir(), fun analyze_file/2, {BaseLen, 0, #{}}).

%% TEST-API

test_traverse_add() ->
    traverse(base_dir(), fun add/2, []).

test_traverse_sum() ->
    traverse(base_dir(), fun sum/2, 0).

test_file() ->
    get_it_all(url_test_file()).

test_file2() ->
    get_it_all(url_test_file2()).

%% Internal

traverse(Dir, Fun, Acc) ->
    filelib:fold_files(Dir, ".*url", true, Fun, Acc).

analyze_file(Filepath, {BaseLen, Index, Acc}) ->
    erlang:display(Index),
    {_,RelFilepath} = lists:split(BaseLen, Filepath),
    RelDir = filename:dirname(RelFilepath),
    Filename = filename:basename(RelFilepath, ".url"),
    [Artist | _] = filename:split(RelDir),
    Map0 =
	#{index => Index,
	  filepath => Filepath,
	  rel_filepath => RelFilepath,
	  filename => Filename,
	  artist => Artist},
    Map =
	try
	    {ok, Url} = parse_url_file(Filepath),
	    Map1 = Map0#{url => Url},
	    try
		{ok, {Channel, Owner}} = get_channel(Url),
		erlang:display({ok, {Artist,
				     Channel}}),
		Map1#{channel => Channel,
		      owner => Owner}
	    catch Type:Reason:_Trace ->
		    erlang:display({error, {channel,
					    Artist,
					    Reason,
					    Filename}}),
		    Map1#{type => Type,
			  reason => Reason}
	    end
	catch Type2:Reason2:_Trace2 ->
		erlang:display({error, {url,
					Artist,
					Reason2,
					Filename}}),
		Map0#{type => Type2,
		      reason => Reason2}
	end,
    {BaseLen, Index+1, Acc#{Index => Map}}.

get_it_all(Filepath) ->
    {ok, Url} = parse_url_file(Filepath),
    {ok, {Channel, Owner}} = get_channel(Url),
    {Url, Channel, Owner}.

get_channel(Url) ->
    {ok, {{_,200,"OK"}, _, Body}} = httpc:request(Url),
    find_channel(Body).

parse_url_file(File) ->
    {ok, Content} = file:read_file(File),
    Lines = binary:split(Content, [<<"\n">>, <<"\r">>], [global, trim_all]),
    find_url(Lines).

add(Filepath, Acc) ->
    [Filepath | Acc].

sum(_Filepath, Acc) ->
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
    "/mnt/c/Users/roland/Desktop/musik".

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
