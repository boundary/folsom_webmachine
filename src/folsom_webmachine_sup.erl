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
%%% File:      folsom_webmachine_sup.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc Supervisor for the folsom application.
%%% @end
%%%------------------------------------------------------------------

-module(folsom_webmachine_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

-include("folsom_webmachine.hrl").

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->

    Ip = case os:getenv("FOLSOM_IP") of false -> "127.0.0.1"; Any -> Any end,
    Port = case os:getenv("FOLSOM_PORT") of false -> "5565"; Any1 -> Any1 end,
    LogDir = case os:getenv("FOLSOM_LOG_DIR") of false -> "priv/log"; Any2 -> Any2 end,

    WebConfig = [
                 {ip, Ip},
                 {port, Port},
                 {log_dir, LogDir},
                 {dispatch, ?DISPATCH}
                ],

    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent,
           5000,
           worker,
           dynamic},

    {ok, { {one_for_one, 10, 10}, [Web]} }.
