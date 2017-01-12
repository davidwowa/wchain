%% @author David
%% @doc @todo Add description to my_sha.


-module(my_sha).

%% ====================================================================
%% API functions
%% ====================================================================
-export([generate_sha/3]).
-export([sha_hex/1]).
%% ====================================================================
%% Internal functions
%% ====================================================================

generate_sha(_, Context, 1000) -> 
	Digest = crypto:hash_final(Context),
	{lists:flatten(list_to_hex(binary:bin_to_list(Digest))), 1000};
generate_sha(Value, Context, Count) ->
	New_context = crypto:hash_update(Context, Value),
	Diggest = crypto:hash_final(New_context),
	List = lists:flatten(list_to_hex(binary:bin_to_list(Diggest))),
%% 	lager:info("MD5: ~p ~p \n", [List, Count]),
	Start = string:sub_string(List, 1, 2),
	if
		Start == "00"->
			{List, Count};
		true -> generate_sha(Value, New_context, Count+1)
	end.
	
%% See http://sacharya.com/tag/integer-to-hex-in-erlang/
sha_hex(S) ->
	SHA_bin =  crypto:hash(sha256, S),
	SHA_list = binary_to_list(SHA_bin),
	lists:flatten(list_to_hex(SHA_list)).

list_to_hex(L) ->
	lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
	[hex(N div 16), hex(N rem 16)].
hex(N) when N < 10 ->
	$0+N;
hex(N) when N >= 10, N < 16 ->
	$a + (N-10).