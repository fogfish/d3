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
%%   direct distributed dets interface. 
%%
%%   The application implements dets i/o interface so that cluster node
%%   can perform dets i/o using Erlang distribution without any needs to use
%%   rpc or wrapper servers.
%%
%%   see http://www.erlang.org/doc/man/dets.html
-module(d3).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-export([
   start_link/2
  ,open_file/2
  ,close/1
  ,pid/1
  % dets interface
  ,delete/2
  ,delete_object/2
  ,insert/2
  ,insert_new/2
  ,info/1
  ,info/2
  ,lookup/2
  ,member/2
  ,sync/1
  ,update_counter/3
]).

-define(NET_TIMEOUT,  30000).

%%
%% creates dets owner process and opens table.
%% returns pid of dets container 
%% use d3:pid(Name) to get dets i/o process
-spec(start_link/2 :: (atom(), list()) -> {ok, pid()} | {error, any()}).

start_link(Name, Opts) ->
   d3_proc:start_link(Name, Opts).

%%
%% creates dets owner process and opens table. The process
%% is attached to application supervisor.
%% returns pid of dets container process
%% use d3:pid(Name) to get dets i/o process
-spec(open_file/2 :: (atom(), list()) -> {ok, pid()} | {error, any()}).

open_file(Name, Opts) ->
   supervisor:start_child(d3_sup, [Name, Opts]).

%%
%% closes dets tables and terminates owner process
-spec(close/1 :: (atom()) -> ok).

close(Name)
 when is_atom(Name) ->
   gen_server:call(Name, close).


%%
%% retrun pid of dets process
pid(Name) ->
   case dets_server:get_pid(Name) of
      Pid when is_pid(Pid) ->
         Pid;
      _ ->
         exit(badarg)
   end.

%%%----------------------------------------------------------------------------   
%%%
%%% dets interface
%%%
%%%----------------------------------------------------------------------------   

%% 
%% deletes all objects with the key Key from the table.
-spec(delete/2 :: (pid(), any()) -> ok | {error, any()}).

delete(Pid, Key) ->
   request(Pid, {delete_key, [Key]}).

%%
%% deletes all instances of a given object from a table.
-spec(delete_object/2 :: (pid(), any()) -> ok | {error, any()}).

delete_object(Pid, Obj) ->
   request(Pid, {delete_object, [Obj]}).

%%
%% inserts one or more objects into the table.
-spec(insert/2 :: (pid(), tuple() | [tuple()]) -> ok | {error, any()}).

insert(Pid, Objs)
 when is_list(Objs) ->
   request(Pid, {insert, Objs});
insert(Pid, Obj) ->
   request(Pid, {insert, [Obj]}).

%%
%% inserts one or more objects into the table. returns false if object exists
-spec(insert_new/2 :: (pid(), tuple() | [tuple()]) -> boolean()).

insert_new(Pid, Objs)
 when is_list(Objs) ->
   request(Pid, {insert_new, Objs});
insert_new(Pid, Obj) ->
   request(Pid, {insert_new, [Obj]}).

%%
%% returns information about the table
%% see dets api for details
-spec(info/1 :: (pid()) -> list()).
-spec(info/2 :: (pid(), atom()) -> list()).

info(Pid) ->
   request(Pid, info).

info(Pid, Tag) ->
   request(Pid, {info, Tag}).


%%
%% returns a list of all objects with the key Key stored in the table.
-spec(lookup/2 :: (pid(), any()) -> [tuple()] | {error, any()}).

lookup(Pid, Key) ->
   request(Pid, {lookup_keys, [Key]}).

%%
%% returns true if one or more elements of the table has the key Key, false otherwise.
-spec(member/2 :: (pid(), any()) -> boolean() | {error, any()}).

member(Pid, Key) ->
   request(Pid, {member, Key}). 

%%
%% ensures that all updates made to the table are written to disk.
-spec(sync/1 :: (pid()) -> ok | {error, any()}).

sync(Pid) ->
   request(Pid, sync).

%%
%% updates the counter object with key Key stored in the table.
-spec(update_counter/3 :: (pid(), any(), {integer(), integer()} | integer()) -> integer()).

update_counter(Pid, Key, C) ->
   request(Pid, {update_counter, Key, C}).


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% issue direct dets request
request(Pid, Req)
 when is_pid(Pid) ->
   case erlang:node(Pid) of
   Node when Node =:= erlang:node() ->
      do_request(Pid, Req);
   _ ->
      do_request(Pid, Req, ?NET_TIMEOUT)
  end.


do_request(Pid, Req) ->
   Tx = erlang:monitor(process, Pid),
   %% @see dets.hrl for request format
   catch erlang:send(Pid, {'$dets_call', self(), Req}, [noconnect]),
   receive 
      {Pid, Reply} ->
         erlang:demonitor(Tx, [flush]),
         Reply;
      {'DOWN', Tx, _, _, noconnection} ->
         exit({nodedown, erlang:node(Pid)});
      {'DOWN', Tx, _, _, Reason} ->
         exit(Reason)
   end.

do_request(Pid, Req, Timeout) ->
   Tx = erlang:monitor(process, Pid),
   %% @see dets.hrl for request format
   catch erlang:send(Pid, {'$dets_call', self(), Req}, [noconnect]),
   receive 
      {Pid, Reply} ->
         erlang:demonitor(Tx, [flush]),
         Reply;
      {'DOWN', Tx, _, _, noconnection} ->
         exit({nodedown, erlang:node(Pid)});
      {'DOWN', Tx, _, _, Reason} ->
         exit(Reason)
   after Timeout ->
      erlang:demonitor(Tx, [flush]),
      exit(timeout)
   end.

