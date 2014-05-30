%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 27 Mar 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_statsd).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([connect/0, put/5]).
-ignore_xref([put/5]).
-record(statsd, {enabled}).

connect() ->
    case application:get_env(tachyon, statsd) of
        {ok, true} ->
            {ok, #statsd{enabled=true}};
        _ ->
            {ok, #statsd{enabled=false}}
    end.


put(Metric, Value, Time, Args, S=#statsd{enabled=true}) ->
    estatsd:gauge(fmt(Metric, Args), Time, Value),
    tachyon_mps:send(),
    S;

put(_Metric, _Value, _Time, _Args, S) ->
    S.

fmt(Metric, Args) ->
    [Metric | fmt_args(lists:reverse(Args), [])].

fmt_args([{_, V}|R], Acc) when is_integer(V) ->
    fmt_args(R, [$., integer_to_list(V) | Acc]);
fmt_args([{_, V}|R], Acc) when is_float(V) ->
    fmt_args(R, [$., float_to_list(V) | Acc]);
fmt_args([{_, V}|R], Acc) when is_binary(V) ->
    fmt_args(R, [<<$., V/binary>> | Acc]);
fmt_args([{_, V}|R], Acc) when is_list(V) ->
    fmt_args(R, [$., V | Acc]);

fmt_args([], Acc) ->
    Acc.

-ifdef(TEST).

fmt_test() ->
    Fmt = fmt(<<"a.metric">>, [{hypervisor, <<"bla">>}, {id, 1}]),
    S = list_to_binary(Fmt),
    ?assertEqual(<<"a.metric.bla.1">>, S).

fmtno_arg_test() ->
    Fmt = fmt(<<"a.metric">>, []),
    S = list_to_binary(Fmt),
    ?assertEqual(<<"a.metric">>, S).

-endif.
