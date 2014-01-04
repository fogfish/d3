# Direct Distributed DETS interface

The application implements dets i/o interface so that cluster node
can perform dets i/o using Erlang distribution without any needs to use
rpc or wrapper servers.

see http://www.erlang.org/doc/man/dets.html


```erlang
	{ok, Pid} = d3:open_file(my_table, [{file, "/tmp/my_table"}]).
	
	d3:insert(Pid, {1, "my data"}).
	d3:lookup(Pid, 1).

	%% Pid of dets process can be communicated to Erlang cluster nodes
	%% Erlang distribution would take care of IPC

	d3:close(my_table).
```


