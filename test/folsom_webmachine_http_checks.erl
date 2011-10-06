%%%
%%% Copyright 2011, Boundary
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%


%%%-------------------------------------------------------------------
%%% File:      folsom_webmachine_http_checks.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom_webmachine_http_checks).

-include_lib("eunit/include/eunit.hrl").

-export([run/0]).

-define(SYSTEM_URL, "http://localhost:5565/_system").
-define(STATISTICS_URL, "http://localhost:5565/_statistics").
-define(MEMORY_URL, "http://localhost:5565/_memory").
-define(BASE_METRICS_URL, "http://localhost:5565/_metrics").

run() ->
    metrics_checks(),

    system_checks(),
    statistics_checks(),
    memory_checks(),

    create_metric(),
    populate_metric(),
    delete_metric().

metrics_checks() ->
    Body1 = http_helpers:http_get(?BASE_METRICS_URL),
    List1 = mochijson2:decode(Body1),
    true = lists:member(<<"counter">>, List1),
    ?debugFmt("http: ~p~n", [List1]),

    ?debugFmt("erlang: ~p~n", [folsom_metrics:get_metrics()]),

    Url1 = lists:append(io_lib:format("~s~s~p", [?BASE_METRICS_URL, "/", counter])),
    Body2 = http_helpers:http_get(Url1),
    {struct, List2} = mochijson2:decode(Body2),
    0 = proplists:get_value(<<"value">>, List2),

    Url2 = lists:append(io_lib:format("~s~s~p", [?BASE_METRICS_URL, "/", gauge])),
    Body3 = http_helpers:http_get(Url2),
    {struct, List3} = mochijson2:decode(Body3),
    2 = proplists:get_value(<<"value">>, List3).

system_checks() ->
    % check _system stats
    Body1 = http_helpers:http_get(?SYSTEM_URL),
    {struct, List1} = mochijson2:decode(Body1),

    % verify one of the many keys exist
    true = lists:keymember(<<"allocated_areas">>, 1, List1).

statistics_checks() ->
    % check _statistics stats
    Body1 = http_helpers:http_get(?STATISTICS_URL),
    {struct, List1} = mochijson2:decode(Body1),

    % verify one of the many keys exist
    true = lists:keymember(<<"context_switches">>, 1, List1).

memory_checks() ->
    % check _memory stats
    Body1 = http_helpers:http_get(?MEMORY_URL),
    {struct, List1} = mochijson2:decode(Body1),

    % verify one of the many keys exist
    true = lists:keymember(<<"total">>, 1, List1).

create_metric() ->
    Proplist = [
                {id, "http"},
                {type, "counter"}
               ],

    Body = mochijson2:encode(Proplist),
    ok = http_helpers:http_put(?BASE_METRICS_URL, Body),

    ?debugFmt("erlang: ~p~n", [folsom_metrics:get_metrics()]),

    Result = http_helpers:http_get(lists:append(io_lib:format("~s~s", [?BASE_METRICS_URL, "/http"]))),
    {struct, [{<<"value">>, 0}]} = mochijson2:decode(Result).

populate_metric() ->
    Proplist = [
                {value, [{inc, 1}]}
               ],

    Body = mochijson2:encode(Proplist),
    ok = http_helpers:http_put(lists:append(io_lib:format("~s~s", [?BASE_METRICS_URL, "/http"])), Body),

    Result = http_helpers:http_get(lists:append(io_lib:format("~s~s", [?BASE_METRICS_URL, "/http"]))),
    {struct, [{<<"value">>, 1}]} = mochijson2:decode(Result).

delete_metric() ->
    Url1 = lists:append(io_lib:format("~s~s", [?BASE_METRICS_URL, "/http"])),
    ok = http_helpers:http_delete(Url1).
