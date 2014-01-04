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
%% @description
%%
-module(d3_proc).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-behaiviour(gen_server).

-export([
   start_link/2,
   init/1, 
   handle_call/3, 
   handle_cast/2, 
   handle_info/2, 
   terminate/2, 
   code_change/3
]).

%% internal state
-record(srv, {
	name = undefined :: atom()
}).


%%%----------------------------------------------------------------------------   
%%%
%%% Factory
%%%
%%%----------------------------------------------------------------------------   

%%
%% 
start_link(Name, Opts) ->
	gen_server:start_link({local, Name}, ?MODULE, [Name, Opts], []).

init([Name, Opts]) ->
	{ok, _} = dets:open_file(Name, Opts),
	{ok, 
		#srv{
			name = Name
		}
	}.

terminate(_, S) ->
	_ = dets:close(S#srv.name),
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% gen_server
%%%
%%%----------------------------------------------------------------------------   

%%
%%
handle_call(close, _, S) ->
	{stop, normal, ok, S};

handle_call(_, _, S) ->
   {noreply, S}.

%%
%%
handle_cast(_, S) ->
   {noreply, S}.

%%
%%
handle_info(_, S) ->
   {noreply, S}.

%%
%%
code_change(_Vsn, S, _) ->
   {ok, S}.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

