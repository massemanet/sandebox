%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 20 Jan 2014 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('sandebox').
-author('mats cronqvist').

%% mod_fun callback
-export([do/2]).

%% the API
-export([start/0,stop/0,state/0,unlink/0]).

%% for application supervisor
-export([start_link/0]).

%% gen_server callbacks
-behaviour(gen_server).
-export([init/1,terminate/2,code_change/3,
         handle_call/3,handle_cast/2,handle_info/2]).

%% declare the state
-record(b,{uno = 1,due = [1,2]}).
-record(state,{a = {},b = #b{}}).

%% add all records here, to kludge around the record kludge.
rec_info(state) -> record_info(fields,state);
rec_info(b)     -> record_info(fields,b);
rec_info(_)     -> [].

%% the API
start() ->
  application:start(?MODULE).

stop() ->
  application:stop(?MODULE).

unlink() ->
  gen_server:call(?MODULE,unlink).

state() ->
  gen_server:call(?MODULE,state).

%% for application supervisor
start_link() ->
  [inets:start() || not is_started(inets)],
  inets:start(httpd,conf()).

is_started(A) ->
  lists:member(A,[X || {X,_,_} <- application:which_applications()]).

conf() ->
  Root =
    case application:get_env(kernel,error_logger) of
      {ok,{file,File}} -> filename:dirname(File);
      _ -> filename:join("/tmp",?MODULE)
    end,
  [{port, 8765},
   {server_name,atom_to_list(?MODULE)},
   {server_root,ensure(Root)},
   {document_root,ensure(Root)},
   {modules, [mod_alias,mod_fun,mod_log]},
   {error_log,filename:join(ensure(Root),"errors.log")},
   {handler_function,{?MODULE,do}},
   {mime_types,[{"html","text/html"},
                {"css","text/css"},
                {"ico","image/x-icon"},
                {"js","application/javascript"}]}].

ensure(X) ->
  filelib:ensure_dir(X++"/"),
  X.

%% called from mod_fun. runs in a fresh process.
%% Req is a dict with the request data from inets. It is implemented
%% as a fun/1, with the arg being the key in the dict.
%% we can deliver the content in chunks by calling Act(Chunk).
%% the first chunk can be headers; [{Key,Val}]
%% if we don't want to handle the request, we do Act(defer)
%% if we crash, there will be a 404.
do(Act,Req) ->
  case {Req(method),string:tokens(Req(request_uri),"/")} of
    {"GET", []} -> Act(ship("index.html"));
    {"POST", U} -> Act(flat({U,run(html_id(Req(entity_body)))}));
    {M,P}       -> Act("sandebox default: "++M++": "++P)
  end.

flat(T) ->
  lists:flatten(io_lib:fwrite("~p",[T])).

run(Str) ->
  execute(compile(decode(Str))).

execute({Mod,Bin}) ->
  {Mod,Bin}.

compile(Str) ->
  try
    {ok, Toks, _} = erl_scan:string(Str),
    Parses = [parse_form(Form) || Form <- split_into_forms(Toks)],
    {ok, Mod, Binary} = compile:forms(Parses),
    code:load_binary(Mod, "", Binary)
  catch
    _:R -> {R,erlang:get_stacktrace()}
  end.

parse_form(Form) ->
  {ok,Parse} = erl_parse:parse_form(Form),
  Parse.

split_into_forms(Toks) ->
  lists:reverse(split_into_forms(Toks,[[]])).
split_into_forms([{dot,L}|Toks],[H|T]) ->
  split_into_forms(Toks,[[],H++[{dot,L}]|T]);
split_into_forms([Tok|Toks],[H|T]) ->
  split_into_forms(Toks,[H++[Tok]|T]);
split_into_forms([],[[]|T]) ->
  T;
split_into_forms([],_) ->
  exit(missing_dot_at_eof).

decode(Str) -> http_uri:decode(plus_to_space(Str)).

plus_to_space([$+|R]) -> [$ |plus_to_space(R)];
plus_to_space([H|R])  -> [H|plus_to_space(R)];
plus_to_space([])     -> [].

html_id(Str) ->
  ["code",Code] = string:tokens(Str,"="),
  Code.

ship(File) ->
  {ok,F} = file:read_file(static(File)),
  F.

static(File) ->
  filename:join(static(),File).

static() ->
  filename:join(code:priv_dir(sandebox),static).

%% gen_server callbacks
init(_) ->
  {ok,#state{}}.

terminate(_Reason,_State) ->
  ok.

code_change(_OldVsn,State,_Extra) ->
  {ok,State}.

handle_call(state,_From,State) ->
  {reply,expand_recs(State),State};
handle_call(unlink,_From,State) ->
  {links,Links} = process_info(self(),links),
  lists:foreach(fun unlink/1,Links),
  {reply,ok,State};
handle_call(What,_From,State) ->
  {reply,What,State}.

handle_cast(_What,State) ->
  {noreply,State}.

handle_info(_What,State) ->
  {noreply,State}.

%% utility to print state
expand_recs(List) when is_list(List) ->
  [expand_recs(I) || I <- List];
expand_recs(Tup) when is_tuple(Tup) ->
  case tuple_size(Tup) of
    L when L < 1 -> Tup;
    L ->
      try Fields = rec_info(element(1,Tup)),
          L = length(Fields)+1,
          lists:zip(Fields,expand_recs(tl(tuple_to_list(Tup))))
      catch _:_ ->
          list_to_tuple(expand_recs(tuple_to_list(Tup)))
      end
  end;
expand_recs(Term) ->
  Term.

%% end of boilerplate
