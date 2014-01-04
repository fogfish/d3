%%
%%   Copyright (c) 2014, Dmitry Kolesnikov
%%   All Rights Reserved.
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%%   @description
%%      basho_bench driver
-module(d3_benchmark).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-export([
   start/0
  ,new/1
  ,run/4
]).

-define(NODE,  'd3@127.0.0.1').

-define(INSERT(P, X), d3:insert(P, X)).
-define(LOOKUP(P, X), d3:lookup(P, X)).
-define(DELETE(P, X), d3:delete(P, X)).

% -define(INSERT(P, X), rpc:call(?NODE, dets, insert, [d3t, X])).
% -define(LOOKUP(P, X), rpc:call(?NODE, dets, lookup, [d3t, X])).
% -define(DELETE(P, X), rpc:call(?NODE, dets, delete, [d3t, X])).


%%
%% start application
start() ->
   _ = application:start(d3),
   _ = d3:open_file(d3t, [{file, "/tmp/d3t"}]).


%%
%%
new(_Id) ->
   {ok, rpc:call(?NODE, d3, pid, [d3t])}.

%%
%%
run(insert, KeyGen, ValGen, Pid) ->
   case (catch ?INSERT(Pid, {KeyGen(), ValGen()})) of
      ok -> {ok, Pid};
      _  -> {error, put, Pid}
   end;

run(lookup, KeyGen, _ValGen, Pid) ->
   case (catch ?LOOKUP(Pid, KeyGen())) of
      X when is_list(X) -> {ok, Pid};
      _  -> {error, lookup, Pid}
   end;

run(delete, KeyGen, _ValGen, Pid) ->
   case (catch ?DELETE(Pid, KeyGen())) of
      ok -> {ok, Pid};
      _  -> {error, delete, Pid}
   end.


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   


