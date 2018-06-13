# Emeter
Erlang/Elixir Web based and pluginable metrics, monitoring, and observer.  


## Features
* Monitor Erlang virtual machine (<a href="https://user-images.githubusercontent.com/20663776/40584187-b4356222-618c-11e8-9cf2-92329747bec8.png" target="_blank">screenshot</a>):  
    * Memory:  
        * Total used memory  
        * Total used memory for processes  
        * Total used memory for ETS tables  
        * Total used memory for atoms  
        * ...  
    * Statistics:  
        * Process count  
        * Virtual machine I/O  
        * Atom count  
        * Run queue  
        * ...  
    * CPU:  
        * Online scheduler count  
        * Scheduler(s) load average  
        * Logical CPU count  
        * ...  
    * Erlang information:  
        * OTP version  
        * ERTS version  
        * Async thread count  
        * ...  
* Graphs for CPU(s) load average, Memory, I/O, ... (<a href="https://user-images.githubusercontent.com/20663776/40584189-b8e0e2ce-618c-11e8-92de-aaf8fc618696.png" target="_blank">screenshot</a>)
* See application's supervison tree with different colors for different process behaviors (<a href="https://user-images.githubusercontent.com/20663776/40584192-be753b86-618c-11e8-8f70-dc98b1ae950c.png" target="_blank">screenshot</a>). 
* See important information for different processes (<a href="https://user-images.githubusercontent.com/20663776/40584195-c285ffee-618c-11e8-85cc-8318b6f57714.png" target="_blank">screenshot</a>).
* See information about each ETS table and its data (<a href="https://user-images.githubusercontent.com/20663776/40584196-c4f03cae-618c-11e8-8a87-a4d65063f265.png" target="_blank">screenshot</a>).
* Run Emeter as stand-alone Erlang release and connect it to different Erlang nodes and monitor them without having Emeter installed on them.  you can also use it as a dependency inside other releases.  
* Write your own plugins or add your own pages to web panel for collecting your own data.  
* Have ReST API for all above features.  

All features not listed here. 


## Build
```sh
/projects $ git clone https://github.com/pouriya-jahanbakhsh/emeter && cd emeter
...
/projects/emeter $ make all
```
Not that for `make all` it rebuilds web assets and you should have `npm` and `nodejs` installed. If you just want Erlang code compilation, use `make compile`.  
By default Emeter uses `error_logger` for generating logs, if you want to use `lager` instead, export environment variable `ERL_EMETER_USE_LAGER` and recompile code.
```sh
/projects/emeter $ export ERL_EMETER_USE_LAGER=1
/projects/emeter $ make compile
```
Also it uses `jsone` for JSON decoding/encoding. If you want to use `jiffy`, export environment variable `ERL_EMETER_USE_JIFFY` and recompile code.  
If you want to use custom `inetrc` file for Erlang distribution, Use `make release inetrc=/path/to/your/inetrc`.  
For more information see other targets of `Makefile`.  


## ReST API
In below examples `NODE_NAME` should be an Erlang node (e.g. `mynode@myhost`) or `local`. Each JSON object has key `ok` with boolean value. If value is `false`, there is also a key named `error` with details about failure. Otherwise all collected data is in key `data`. Also i used comments after `//` to explain type of some values. 

#### System
Request path:  
```sh
/api/NODE_NAME/system
```
Response example:  
```javascript
{
    "data": {
        "architecture": {
            "erts_version": "8.3",
            "kernel_poll": true,
            "otp_release": "19",
            "smp_support": true,
            "system_architecture": "x86_64-unknown-linux-gnu",
            "thread_pool_size": 0,
            "threads": true,
            "wordsize_external": 8,
            "wordsize_internal": 8
        },
        "cpu": {
            "logical_processors": 8,
            "logical_processors_available": 8,
            "logical_processors_online": 8,
            "schedulers": 8,
            "schedulers_available": 8,
            "schedulers_online": 8
        },
        "memory": {
            "atom": 695209, // in bytes
            "binary": 453640, // in bytes
            "code": 16340117, // in bytes
            "ets": 745752, // in bytes
            "process": 16085720, // in bytes
            "total": 55962824 // in bytes
        },
        "scheduler": [
            1.02776108895425869410e-05, // Calculating percentage: erlang:trunc(Number * 100)
            1.22620961782036209931e-05,
            9.76175533590747864258e-06,
            1.02240399623291754048e-05,
            9.64168932117280918603e-06,
            5.39933812650289545257e-04,
            8.16349106933836378701e-06,
            7.62789294826068677127e-06
        ],
        "statistics": {
            "input": 23012006, // in bytes
            "output": 1093897, // in bytes
            "atom_count": 21830, 
            "process_max": 1048576, 
            "process_running": 0,
            "process_total": 105,
            "uptime": 1527988 // in milli-seconds
        }
    },
    "ok": true,
    "timestamp": 1527412926 // in seconds. used by web panel
}
```


#### All applications (which have supervision tree)
Request path:  
```sh
/api/NODE_NAME/application
```
Response example:  
```javascript
{
    "data": [
        {
            "description": "SASL  CXC 138 11",
            "name": "sasl",
            "version": "3.0.3"
        },
        {
            "description": "Erlang/Elixir Web based and pluginable metrics, monitoring, and observer.",
            "name": "emeter",
            "version": "0.1.0"
        },
        {
            "description": "Erlang logging framework",
            "name": "lager",
            "version": "3.6.2"
        },
        {
            "description": "Small, fast, modern HTTP server.",
            "name": "cowboy",
            "version": "2.3.0"
        },
        {
            "description": "Socket acceptor pool for TCP protocols.",
            "name": "ranch",
            "version": "1.4.0"
        },
        {
            "description": "Erlang/OTP SSL application",
            "name": "ssl",
            "version": "8.1.1"
        },
        {
            "description": "Erlang event stream processor",
            "name": "goldrush",
            "version": "0.1.9"
        },
        {
            "description": "ERTS  CXC 138 10",
            "name": "kernel",
            "version": "5.2"
        }
    ],
    "ok": true,
    "timestamp": 1527417065
}
```
#### One application
Request path:  
```csh
/api/NODE_NAME/application/APPLICATION_NAME
```
Response example for `cowboy` application:
```javascript
{
    "data": {
        "children": [
            {
                "children": [
                    {
                        "children": [],
                        "meta": {
                            "behaviour": "gen_server",
                            "current": "gen_server:loop/6",
                            "init": "cowboy_clock:init/1",
                            "status": "waiting"
                        },
                        "name": "<0.494.0>",
                        "pid": "<0.494.0>"
                    }
                ],
                "meta": {
                    "behaviour": "supervisor",
                    "current": "gen_server:loop/6",
                    "init": "supervisor:cowboy_sup/1",
                    "status": "waiting"
                },
                "name": "<0.493.0>",
                "pid": "<0.493.0>"
            }
        ],
        "meta": {
            "behaviour": "application",
            "current": "application_master:main_loop/2",
            "init": "application_master:init/4",
            "status": "waiting"
        },
        "name": "<0.491.0>",
        "pid": "<0.491.0>"
    },
    "ok": true,
    "timestamp": 1527417950
}
```


#### All registered processes
Request path:  
```sh
/api/NODE_NAME/process
```
Response example:  
```javascript
{
    "data": [
        {
            "current": "gen_server:loop/6",
            "init": "file_server:init/1",
            "memory": 0,
            "message_queue_length": 0,
            "name": "file_server_2",
            "pid": "<0.447.0>",
            "reductions": "1109"
        },
        {
            "current": "gen_server:loop/6",
            "init": "supervisor:kernel/1",
            "memory": 0,
            "message_queue_length": 0,
            "name": "kernel_safe_sup",
            "pid": "<0.456.0>",
            "reductions": "64"
        },
        {
            "current": "gen_server:loop/6",
            "init": "cowboy_clock:init/1",
            "memory": 0,
            "message_queue_length": 0,
            "name": "cowboy_clock",
            "pid": "<0.494.0>",
            "reductions": "3954"
        },
        {
            "current": "gen_server:loop/6",
            "init": "supervisor:gr_counter_sup/1",
            "memory": 0,
            "message_queue_length": 0,
            "name": "gr_counter_sup",
            "pid": "<0.468.0>",
            "reductions": "107"
        },
        // ...
    ],
    "ok": true,
    "timestamp": 1527418135
}
```

#### One process
Request path:  
```sh
/api/NODE_NAME/process/PID_OR_REGISTERED_NAME
```
Response example for pid `<0.0.0>` (`init` process):  
```javascript
{
    "data": {
        "error_handler": "error_handler",
        "memory": {
            "gc_full_sweep_after": 0,
            "gc_min_heap_size": 233, // in words
            "heap_size": 987, // in words
            "stack_and_heap": 1974, // in words
            "stack_size": 2, // in words
            "total": 1974 // in words
        },
        "message_queue_len": 0,
        "meta": {
            "behaviour": "unknown",
            "current": "init:loop/1",
            "init": "otp_ring0:start/2",
            "status": "waiting"
        },
        "pid": "<0.0.0>",
        "priority": "normal",
        "registered_name": "init",
        "relations": {
            "ancestors": [],
            "group_leader": "<0.0.0>",
            "links": [
                "<0.426.0>",
                "<0.427.0>",
                "<0.4.0>"
            ]
        },
        // This process did not handle system messages. So we can't get its state in normal fashion.
        "state": "",
        "trap_exit": true
    },
    "ok": true,
    "timestamp": 1527418325
}
```


#### All Ports
Request path:  
```csh
/api/NODE_NAME/ports
```
Response example:  
```javascript
{
    "data": [
        {
            "connected": "<0.655.0>",
            "id": 7480,
            "input": 0, // in bytes
            "links": [
                "<0.655.0>"
            ],
            "name": "tcp_inet",
            "os_pid": "undefined", // or pid in integer
            "output": 0, // in bytes
            "port": "#Port<0.935>"
        },
        {
            "connected": "<0.654.0>",
            "id": 7472,
            "input": 0,
            "links": [
                "<0.654.0>"
            ],
            "name": "tcp_inet",
            "os_pid": "undefined",
            "output": 0,
            "port": "#Port<0.934>"
        },
        {
            "connected": "<0.652.0>",
            "id": 7464,
            "input": 0,
            "links": [
                "<0.652.0>"
            ],
            "name": "tcp_inet",
            "os_pid": "undefined",
            "output": 0,
            "port": "#Port<0.933>"
        },
        // ...
    ],
    "ok": true,
    "timestamp": 1527419207
}
```


#### All ETS tables
Request path:  
```sh
/api/NODE_NAME/table
```
Response example:  
```javascript
{
    "data": [
        {
            "id": "ac_tab",
            "memory": 3769, // in words
            "meta": {
                "compressed": "false",
                "read_concurrency": "true",
                "write_concurrency": "false"
            },
            "name": "ac_tab",
            "owner": "<0.427.0>",
            "protection": "public",
            "size": 62,
            "type": "set"
        },
        {
            "id": "1",
            "memory": 67663,
            "meta": {
                "compressed": "false",
                "read_concurrency": "false",
                "write_concurrency": "false"
            },
            "name": "code",
            "owner": "<0.432.0>",
            "protection": "private",
            "size": 431,
            "type": "set"
        },
        {
            "id": "4098",
            "memory": 3089,
            "meta": {
                "compressed": "false",
                "read_concurrency": "false",
                "write_concurrency": "false"
            },
            "name": "code_names",
            "owner": "<0.432.0>",
            "protection": "private",
            "size": 17,
            "type": "set"
        },
        // ...
    ],
    "ok": true,
    "timestamp": 1527419427
}
```

#### One ETS table
Request path:  
```sh
/api/NODE_NAME/table/TABLE_ID_OR_NAME
```
Response example for `lager_config`:  
```javascript
{
    "data": {
        "data": [
            [
                "{lager_event,async}",
                "true"
            ],
            [
                "{lager_event,loglevel}",
                "{127,[]}"
            ],
            [
                "{'_global',handlers}",
                "[{lager_console_backend,<0.504.0>,lager_event},\n {{lager_file_backend,\"log/error.log\"},<0.506.0>,lager_event},\n {{lager_file_backend,\"log/console.log\"},<0.508.0>,lager_event}]"
            ]
        ],
        "id": "lager_config",
        "memory": 411,
        "meta": {
            "compressed": "false",
            "read_concurrency": "true",
            "write_concurrency": "false"
        },
        "name": "lager_config",
        "owner": "<0.498.0>",
        "protection": "public",
        "size": 3,
        "type": "set"
    },
    "ok": true,
    "timestamp": 1527419694
}
```

#### Virtual machine allocators
Request path:  
```sh
/api/NODE_NAME/allocators
```
Response example:  
```javascript
{
    "data": [
		// in bytes
        {
            "block": 1112,
            "carrier": 294912,
            "type": "sl_alloc"
        },
        {
            "block": 180032,
            "carrier": 819200,
            "type": "std_alloc"
        },
        {
            "block": 49482488,
            "carrier": 60555264,
            "type": "ll_alloc"
        },
        {
            "block": 953344,
            "carrier": 2490368,
            "type": "eheap_alloc"
        },
        {
            "block": 696560,
            "carrier": 1605632,
            "type": "ets_alloc"
        },
        {
            "block": 114592,
            "carrier": 557056,
            "type": "fix_alloc"
        },
        {
            "block": 1578688,
            "carrier": 2097152,
            "type": "literal_alloc"
        },
        {
            "block": 51288,
            "carrier": 2129920,
            "type": "binary_alloc"
        },
        {
            "block": 62208,
            "carrier": 557056,
            "type": "driver_alloc"
        }
    ],
    "ok": true,
    "timestamp": 1527416479
}
```

#### List of monitored nodes
Note that if some nodes are disconnected, Emeter does not yield them in response.  
Request path:  
```csh
/api/local/nodes
```
Response example for `lager_config`:  
```javascript
{
    "data": [
        {
            "name": "nodename1@host1"
        },
        {
            "name": "nodename2@host2"
        },
        {
            "name": "nodename2@host2"
        },
        // ...
    ],
    "ok": true,
    "timestamp": 1527420610
}
```


## Connect to other nodes
We can use `emeter:add_node/1` in run-time:  
```erlang
%% Erlang shell (Emeter is running)
(emeter@localhost)1> nodes().
[]

(emeter@localhost)2> emeter_node:all().
[emeter@localhost]

%% I opened new Erlang shell named foo@localhost with same Erlang cookie
(emeter@localhost)3> emeter:add_node(foo@localhost).
ok

(emeter@localhost)4> nodes().                       
[foo@localhost]

(emeter@localhost)5> emeter_node:all().             
[emeter@localhost,foo@localhost]
```

Also we can add node(s) in application environment variables as value of `nodes` which should be list of nodes. `sys.config` example:  
```erlang
[
{emeter, [{nodes, [node2@host2, node2@host2, node3@host3]}]}
]
```
Note that when Emeter connects to a remote Erlang node, It loads module `emeter_agent_api` on that node and starts a `gen_server` process on that node named `emeter_agent_api`. One feature of this process is that allows us to collect useful data from that node simply.  


## Plugins and Pages
Every page should hase a unique name. You should specify a module, say `A` and a function, say `F` for running page too. When you make a request to `/api/NODE_NAME/YOUR_PAGE_NAME`, Emeter runs `M:F(NODE_NAME)` to getting its response. This function has to yield `{'ok', Result}` or `{'error', Reason}`. Note that Emeter tries to encode `Result` to JSON. Final response will be:  
```javascript
{"ok":true, "data": ResultEncodedToJSON}
```
or
```javascript
{"ok":false, "error": ReasonString}
```

For adding a page you can use:
```erlang
emeter_page:add(Name::binary()
               ,Mod::module()
               ,Func::atom()
               ,RefreshTimeout::pos_integer()
               ,APIFlag::boolean())
```
`RefreshTimeout` is a useful feature in web panel. If set `APIFlag` to `false`, you can't see page in web panel and you can just use ReST API.  
Also you can add pages in environment variables of emeter application. `sys.config` example:  
```erlang
[
{emeter, [{pages, [{<<"my_page_name">>, {my_page_module, my_page_function, #{refresh_timeout => 3}}}]}]}
]
```

#### Example
```erlang
-module(my_page_module).
-export([my_page_function/1]).

%% I want to get count of connected nodes 
my_page_function(Node) ->
	{ok, length(rpc:call(Node, erlang, nodes, []))}.
```

## Use efficient and fast pages
Suppose that i want to get all process with high memory (memory > 100KB). I have to write:  
```erlang
-module(my_page_module).
-export([my_page_function/1]).

my_page_function(Node) ->
	%% Get processes of remote node:
	Pids = rpc:call(Node, erlang, processes, []),
	GetMemFoldFun =
		fun(Pid, Acc) ->
			%% I have to get process_info for every pid:
			case rpc:call(Node, erlang, process_info, [Pid, memory]) of
				{memory, M} when M > 102400 ->
					[Pid|Acc];
				_ ->
					Acc
			end
		end,
	HighMemPids = lists:foldl(GetMemFoldFun, [], Pids),
	{ok, [erlang:pid_to_list(X) || X <- HighMemPids]}. %% For encoding to JSON
```
In above i have to load all processes of remote node in current node (if it was remote node). Also i have to call `rpc:call` for every pid too. It's too slow in large systems. There is another way.  
I rewrite above code to:  
```erlang
-module(my_page_module).

-include("/path/to/emeter/include/emeter_plugin.hrl").
-export([my_page_function_pt/0]).

my_page_function_pt() ->
	%% Get processes of node:
	Pids = processes(),
	GetMemFoldFun =
		fun(Pid, Acc) ->
			%% I have to get process_info for every pid:
			case erlang:process_info(Pid, memory) of
				{memory, M} when M > 102400 ->
					[Pid|Acc];
				_ ->
					Acc
			end
		end,
	HighMemPids = lists:foldl(GetMemFoldFun, [], Pids),
	{ok, [erlang:pid_to_list(X) || X <- HighMemPids]}. %% For encoding to JSON
```
After compilation, Emeter transforms every function with arity 0 which its name ends with `_pt`. Now `my_page_module:my_page_function_pt/0` yeilds its AST (Abstract Syntax Tree) and emeter runs its code on requested node using `emeter_agent_api` process. It's too fast and effecient.  

# Plugins
Emeter can start/stop your custom plugin.  You should specify plugins in application environment variables. `sys.config` example:  
```erlang
[
{emeter, [{plugins, [PluginModule :: atom()]}]}
]
```
Emeter calls `PluginModule:start()` which should yield `ok` or `{error, Reason}`.  

### Plugin example
```erlang
-module(my_plugin).

-include("/path/to/emeter/include/emeter_plugin.hrl").
-export([start/0, stop/0, node_count/1, my_page_function_pt/0]).

start() ->
	emeter_page:add(<<"node_count">>
	               ,?MODULE
	               ,node_count
	               ,5
	               ,true),
	emeter_page:add(<<"high_memory_processes">>
	               ,?MODULE
	               ,high_memory_processes_pt
	               ,5
	               ,true),
	ok.

stop() ->
	emeter_page:delete(<<"node_count">>),
	emeter_page:delete(<<"high_memory_processes">>),
	ok.

 
node_count(Node) ->
	{ok, length(rpc:call(Node, erlang, nodes, []))}.


high_memory_processes_pt() ->
	%% Get processes of node:
	Pids = processes(),
	GetMemFoldFun =
		fun(Pid, Acc) ->
			%% I have to get process_info for every pid:
			case erlang:process_info(Pid, memory) of
				{memory, M} when M > 102400 ->
					[Pid|Acc];
				_ ->
					Acc
			end
		end,
	HighMemPids = lists:foldl(GetMemFoldFun, [], Pids),
	{ok, [erlang:pid_to_list(X) || X <- HighMemPids]}. %% For encoding to JSON
```

If your plugin has supervision tree and you want to start it under Emeter's supervision tree, use:  
```erlang
emeter_plugin_sup:start(ChildId :: term()
                       ,StartMod :: module()
                       ,Startfunc :: atom()
                       ,StartArgs :: [] | list())
```
This supervisor will delete your child from its children after 10 crashes.  



