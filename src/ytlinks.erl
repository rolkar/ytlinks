-module(ytlinks).

%% API

-export([run/0,
	 run_including_cached/0]).

%% API running with empty cache, saving nothing

-export([analyze/0,
	 analyze_including_cached/0]).

%% Tests

-export([test_run/0,
	 test_run_including_cached/0,
	 test_analyze/0,
	 test_analyze_including_cached/0,
	 test_traverse_add/0,
	 test_traverse_sum/0,
	 test_file/0,
	 test_file2/0]).

-export([url_test_file2/0]).

%% API

run() ->
    run(base_dir(), exclude_cached).

run_including_cached() ->
    run(base_dir(), include_cached).

%% API running with empty cache, saving nothing

analyze() ->
    analyze(base_dir(), exclude_cached).

analyze_including_cached() ->
    analyze(base_dir(), include_cached).

analyze(BaseDir, Mode) ->
    {_, Cache,
     _, ErrorCache} = get_cache(BaseDir),
    analyze(BaseDir, Mode, Cache, ErrorCache).

%% Internal API

run(BaseDir, Mode) ->
    TableJsonFile = filename:join([BaseDir, "table.json"]),
    IndicesJsonFile = filename:join([BaseDir, "indices.json"]),
    {CacheJsonFile, Cache0,
     ErrorCacheJsonFile, ErrorCache0} = get_cache(BaseDir),

    {US1, {_, _, Num, _, Table, Cache, ErrorCache}} =
	timer:tc(fun() -> analyze(BaseDir, Mode, Cache0, ErrorCache0) end),
    {US2, Indices} = timer:tc(fun() -> build_indices(Table) end),

    write_json(TableJsonFile, Table),
    write_json(IndicesJsonFile, Indices),
    write_json(CacheJsonFile, Cache),
    write_json(ErrorCacheJsonFile, ErrorCache),

    #{<<"analyze_time">> => US1/1000000,
      <<"build_indices_time">> => US2/1000000,
      <<"num">> => Num}.

get_cache(BaseDir) ->
    CacheJsonFile = filename:join([BaseDir, "cache.json"]),
    ErrorCacheJsonFile = filename:join([BaseDir, "error_cache.json"]),
    Cache = read_json(CacheJsonFile),
    ErrorCache = read_json(ErrorCacheJsonFile),
    {CacheJsonFile, Cache, ErrorCacheJsonFile, ErrorCache}.

analyze(BaseDir, Mode, Cache, ErrorCache) ->
    SysTime = erlang:system_time(second),
    StrTime = calendar:system_time_to_rfc3339(SysTime),
    Time = unicode:characters_to_binary(StrTime),
    BaseLen = length(BaseDir) + 1,
    traverse(BaseDir,
	     fun analyze_file/2,
	     {BaseLen, Mode, Time, 0, #{}, Cache, ErrorCache}).

build_indices(DB) ->
    maps:fold(fun insert_indices/3, #{}, DB).

insert_indices(Num, Map, Indices) ->
    {Num, NewIndices} =
	maps:fold(fun insert_index/3, {Num, Indices}, Map),
    NewIndices.

insert_index(K, V, {Num, OldIndices}) ->  
    OldMap = maps:get(K, OldIndices, #{}),
    OldNums = maps:get(V, OldMap, []),
    NewNums = lists:usort([Num|OldNums]),
    NewMap = maps:put(V, NewNums, OldMap),
    NewIndices = maps:put(K, NewMap, OldIndices),
    {Num, NewIndices}.

%% TEST-API

test_run() ->
    run(test_base_dir(), exclude_cached).

test_run_including_cached() ->
    run(test_base_dir(), include_cached).

test_analyze() ->
    analyze(test_base_dir(), exclude_cached).

test_analyze_including_cached() ->
    analyze(test_base_dir(), include_cached).

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
    filelib:fold_files(Dir, ".*\.url$", true, Fun, Acc).

analyze_file(Filepath, {BaseLen, Mode, Time, Num, Acc, Cache, ErrorCache}) ->
    io:format("\r~8p: ", [Num]),
    {_,RelFilepath} = lists:split(BaseLen, Filepath),
    RelDir = filename:dirname(RelFilepath),
    Filename = filename:basename(RelFilepath, ".url"),
    {Artist, IsReaction, MaybeSong} =
	case filename:split(RelDir) of
	    [] ->
		{"", <<"false">>, ""};
	    [Artist0] ->
		{Artist0, <<"false">>, ""};
	    [Artist0, ChildDir | Rest] ->
		case {string:equal("reactions", ChildDir), Rest} of
		    {true, []} ->
			{Artist0, <<"true">>, ""};
		    {true, Rest} ->
			MaybeSong0 = filename:join(Rest),
			{Artist0, <<"true">>, MaybeSong0};
		    {false, Rest} ->
			MaybeSong0 = filename:join([ChildDir|Rest]),
			{Artist0, <<"false">>, MaybeSong0}
		end
	end,
    BinaryNum = integer_to_binary(Num),
    Map0 =
	#{<<"num">> => BinaryNum,
	  <<"filepath">> => unicode:characters_to_binary(Filepath),
	  <<"rel_filepath">> => unicode:characters_to_binary(RelFilepath),
	  <<"filename">> => unicode:characters_to_binary(Filename),
	  <<"artist">> => unicode:characters_to_binary(Artist),
	  <<"is_reaction">> => IsReaction,
	  <<"maybe_song">> => unicode:characters_to_binary(MaybeSong)},
    case parse_url_file(Filepath) of
	{ok, Url} ->
	    Map1 = Map0#{<<"url">> => Url},
	    {CacheWhere, CachedItem} =
		case get_channel(Url, Mode, Cache, ErrorCache) of
		    {cached, Item} ->
			%% io:format("~p~n", [{cached, Item}]),
			{nowhere,
			 Item};
		    {ok, {Channel, Owner}} ->
			io:format("~p~n", [{ok, {Artist,
						 Channel,
						 MaybeSong,
						 IsReaction}}]),
			{cache,
			 #{<<"result">> => ok,
			   <<"channel">> => Channel,
			   <<"owner">> => Owner,
			   <<"time">> => Time}};
		    {error, Reason} ->
			io:format("~p~n", [{error, {channel_failed,
						    Url,
						    Artist,
						    Reason}}]),
			{error_cache,
			 #{<<"result">> => channel_failed,
			   <<"reason">> => Reason,
			   <<"time">> => Time}}
		end,
	    Map = maps:merge(Map1, CachedItem),
	    {NewCache, NewErrorCache} =
		case CacheWhere of
		    cache ->
			{Cache#{Url => CachedItem}, ErrorCache};
		    error_cache ->
			{Cache, ErrorCache#{Url => CachedItem}};
		    nowhere ->
			{Cache, ErrorCache}
		end,
	    {BaseLen , Mode, Time, Num+1, Acc#{BinaryNum => Map}, NewCache, NewErrorCache};
	{error, Reason} ->
	    io:format("~p~n", [{error, {url_failed,
					Artist,
					Reason,
					Filename}}]),
	    Map0#{<<"result">> => url_failed,
		  <<"reason">> => Reason}
    end.

get_it_all(Filepath) ->
    {ok, Url} = parse_url_file(Filepath),
    {ok, {Channel, Owner}} = get_channel(Url),
    {Url, Channel, Owner}.

get_channel(Url) ->
    get_channel(Url, #{}, #{}).

get_channel(Url, Mode, Cache, ErrorCache) ->
    case Mode of
	exclude_cached ->
	    get_channel(Url, Cache, ErrorCache);
	include_cached ->
	    get_channel_remote(Url)
    end.

get_channel(Url, Cache, ErrorCache) ->
    case get_cached_channel(Url, Cache, ErrorCache) of
	notfound ->
	    get_channel_remote(Url);
	{cached, Result} ->
	    {cached, Result}
    end.

get_cached_channel(Url, Cache, ErrorCache) ->
    case maps:get(Url, Cache, notfound) of
	notfound ->
	    case maps:get(Url, ErrorCache, notfound) of
		notfound ->
		    notfound;
		Result ->
		    {cached, Result}
	    end;
	Result ->
	    {cached, Result}
    end.

get_channel_remote(<<"https://www.youtube.com/watch", _/binary>> = Url) ->
    get_channel_remote_checked(Url);
get_channel_remote(<<"https://www.youtube.com/shorts", _/binary>> = Url) ->
    get_channel_remote_checked(Url);
get_channel_remote(<<"https://www.youtube.com/", _/binary>>) ->
    {error, getting_no_youtube_video};
get_channel_remote(_Url) ->
    {error, getting_not_youtube}.

get_channel_remote_checked(Url) ->
    case httpc:request(Url) of
	{ok, {{_,200,"OK"}, _, Body}} ->
	    find_channel(Body);
	{ok, {{_,Reply,_}, _, _}} ->
	    ReplyBinary = integer_to_binary(Reply),
	    {error, <<"http_reply_", ReplyBinary/binary>>};
	_ ->
	    {error, http_failed}
    end.

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
	    Reason = find_error_reason(Body),
	    {error, Reason};
	N ->
	    Rest = lists:sublist(Body, N+PL, 1000),
	    EndPattern = "\"}",
	    case string:str(Rest, EndPattern) of
		0 ->
		    {error, no_end_of_channel};
		M ->
		    Channel = lists:sublist(Rest, 1, M-1),
		    case find_channel_owner(Rest) of
			{ok, ChannelOwner} ->
			    {ok, {unicode:characters_to_binary(Channel),
				  unicode:characters_to_binary(ChannelOwner)}};
			{error, Error} ->
			    {error, Error}
		    end
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

find_error_reason(Body) ->
    StartPattern = "ytInitialPlayerResponse =",
    PL = length(StartPattern),
    case string:str(Body, StartPattern) of
	0 ->
	    no_channel_cause_no_player_response;
	N ->
	    Rest = lists:sublist(Body, N+PL, 10000),
	    EndPattern = ";",
	    case string:str(Rest, EndPattern) of
		0 ->
		    no_channel_cause_no_end_of_player_response;
		M ->
		    InitialPlayerResponseJSON = lists:sublist(Rest, 1, M-1),
		    InitialPlayerResponseJSONbinary =
			unicode:characters_to_binary(InitialPlayerResponseJSON),
		    try InitialPlayerResponse =
			     jsx:decode(InitialPlayerResponseJSONbinary),
			 ErrorReason =
			     extract_error_reason(InitialPlayerResponse),
			 ErrorReason
		    catch _:_ ->
			    no_channel_cause_could_not_decode_player_response
		    end
	    end
    end.

extract_error_reason(IPR) ->
    case maps:get(<<"playabilityStatus">>, IPR, not_found) of
	not_found ->
	    <<"no_channel: cannot_find_playability_status">>;
	PS ->
	    Reason = maps:get(<<"reason">>, PS, "unknown"),
	    ES = maps:get(<<"errorScreen">>, PS, #{}),
	    PEMR = maps:get(<<"playerErrorMessageRenderer">>, ES, #{}),
	    SubreasonContainer = maps:get(<<"subreason">>, PEMR, #{}),
	    Subreason = maps:get(<<"simpleText">>,
				 SubreasonContainer,
				 "unknown"),
	    iolist_to_binary(["no_channel: reason=", Reason,
			      " subreason=", Subreason])
    end.

read_json(File) ->
    case file:read_file(File) of
	{ok, Json} ->
	    try jsx:decode(Json)
	    catch _:_ ->
		    io:format("~p~n", [{parsing_json_failed, File}]),
		    #{}
	    end;
	{error, _} ->
	    io:format("~p~n", [{no_json_file, File}]),
	    #{}
    end.

write_json(File, Data) ->
    Json = jsx:prettify(jsx:encode(Data)),
    file:write_file(File, Json).

base_dir() ->
    "/mnt/c/Users/roland/Desktop/musik".

test_base_dir() ->
    "./test_base_dir".

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
