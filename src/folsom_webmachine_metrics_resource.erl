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
%%% File:      folsom_webmachine_metrics_resource.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% http end point that produces metrics collected from event handlers
%%% @end
%%%------------------------------------------------------------------

-module(folsom_webmachine_metrics_resource).

-export([init/1,
         content_types_provided/2,
         content_types_accepted/2,
         to_json/2,
         from_json/2,
         allowed_methods/2,
         resource_exists/2,
         delete_resource/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {
          key
         }).

init(_) -> {ok, #state{key = undefined}}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", from_json}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET', 'PUT', 'DELETE'], ReqData, Context}.

delete_resource(ReqData, Context) ->
    Id = wrq:path_info(id, ReqData),
    folsom_metrics:delete_metric(list_to_binary(Id)),
    {true, ReqData, Context}.

resource_exists(ReqData, Context) ->
    resource_exists(wrq:path_info(id, ReqData), ReqData, Context).

to_json(ReqData, Context = #state{key = Id}) ->
    Info = wrq:get_qs_value("info", undefined, ReqData),
    CoId = wrq:get_qs_value("metric", undefined, ReqData),
    Result = get_request(Id, Info, CoId),
    {mochijson2:encode(Result), ReqData, Context}.

from_json(ReqData, Context) ->
    {struct, Body} = mochijson2:decode(wrq:req_body(ReqData)),
    Result = case put_request(wrq:path_info(id, ReqData), Body) of
                 ok ->
                     ok;
                 Else ->
                     list_to_binary(Else)
             end,
    {mochijson2:encode(Result), ReqData, Context}.

% internal fuctions

resource_exists(undefined, ReqData, Context) ->
    {true, ReqData, Context};
resource_exists(Id, ReqData, Context) ->
    case metric_exists(Id) of
        {true, Key} ->
            {true, ReqData, Context#state{key = Key}};
        {false, _} ->
            {false, ReqData, Context}
    end.

get_request(undefined, undefined, undefined) ->
    folsom_metrics:get_metrics();
get_request(Id, undefined, undefined) ->
    case folsom_metrics:get_metric_info(Id) of
        [{_, [{type, histogram}]}] ->
            [{value, folsom_metrics:get_histogram_statistics(Id)}];
        _ ->
            [{value, folsom_metrics:get_metric_value(Id)}]
    end;
get_request(Id, undefined, CoId) ->
    [{value,
      folsom_metrics:get_histogram_statistics(list_to_atom(Id), list_to_atom(CoId))}];
get_request(undefined, "true", undefined) ->
    folsom_metrics:get_metrics_info().

put_request(undefined, Body) ->
    Id = list_to_binary(proplists:get_value(<<"id">>, Body)),
    Type = folsom_utils:to_atom(proplists:get_value(<<"type">>, Body)),
    create_metric(Type, Id);
put_request(Id, Body) ->
    BinId = list_to_binary(Id),
    Value = proplists:get_value(<<"value">>, Body),
    Info = folsom_metrics:get_metric_info(BinId),
    Type = proplists:get_value(type, proplists:get_value(BinId, Info)),
    update_metric(Type, BinId, Value).

create_metric(counter, Name) ->
    folsom_metrics:new_counter(Name);
create_metric(gauge, Name) ->
    folsom_metrics:new_gauge(Name);
create_metric(histogram, Name) ->
    folsom_metrics:new_histogram(Name);
create_metric(history, Name) ->
    folsom_metrics:new_history(Name);
create_metric(meter, Name) ->
    folsom_metrics:new_meter(Name).

update_metric(counter, Name, {struct, [{<<"inc">>, Value}]}) ->
    folsom_metrics:notify({Name, {inc, Value}});
update_metric(counter, Name, {struct, [{<<"dec">>, Value}]}) ->
    folsom_metrics:notify({Name, {dec, Value}});
update_metric(gauge, Name, Value) ->
    folsom_metrics:notify({Name, Value});
update_metric(histogram, Name, Value) ->
    folsom_metrics:notify({Name, Value});
update_metric(history, Name, Value) ->
    folsom_metrics:notify({Name, Value});
update_metric(meter, Name, Value) ->
    folsom_metrics:notify({Name, Value}).

% @doc Return true if metric with key 'Id' exists, false otherwise
%
% Searches for a metric with 'Id' stored as a binary first and falls
% back to looking for an existing atom if no matching binary key was
% found.
metric_exists(Id) when is_list(Id) ->
    metric_exists(list_to_binary(Id));
metric_exists(Id) when is_binary(Id) ->
    case folsom_metrics:metric_exists(Id) of
        true  -> {true, Id};
        false ->
            try
                metric_exists(erlang:binary_to_existing_atom(Id, utf8))
            catch
                error:badarg -> {false, Id}
            end
    end;
metric_exists(Id) when is_atom(Id) ->
    {folsom_metrics:metric_exists(Id), Id}.
