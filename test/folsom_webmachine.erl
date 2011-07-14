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
%%% File:      folsom_webmachine_tests.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom_webmachine).

-include_lib("eunit/include/eunit.hrl").

run_test() ->

    application:unset_env(webmachine, dispatch_list),
    folsom_webmachine_sup:start_link(),

    folsom_erlang_checks:create_metrics(),

    folsom_erlang_checks:populate_metrics(),

    ibrowse:start(),
    folsom_webmachine_http_checks:run(),
    ibrowse:stop().

