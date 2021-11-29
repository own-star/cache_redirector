%%%-------------------------------------------------------------------
%% @doc cr public API
%% @end
%%%-------------------------------------------------------------------

-module(cr_app).

-behaviour(application).

-include("cr.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->

    io:setopts([{encoding, utf8}]),

    ets:new(ets_link_to_url, [set, public, named_table]),

    http_server_sup:start_link(),

    Port = ?LISTEN_PORT,

    Dispatch = cowboy_router:compile([{'_', [
       {"/link/[...]", link, []},
       {"/js/[...]", cowboy_static, {priv_dir, cr, "orig/js"}},
       {"/public/js/[...]", cowboy_static, {priv_dir, cr, "orig/public/js"}},
       {"/stylesheets/[...]", cowboy_static, {priv_dir, cr, "orig/stylesheets"}},
       {"/wp-content/[...]", cowboy_static, {priv_dir, cr, "orig/wp-content"}},
       {"/captcha.png", captcha, []},
       {"/[...]", main, []}
    ]}]),
    TransOpts = #{
      socket_opts => [{port, Port}]
    },
    {ok, _} = cowboy:start_clear(http, TransOpts, #{
        stream_handlers => [cowboy_compress_h, cowboy_stream_h],
        env => #{dispatch => Dispatch},
        shutdown_timeout => 300000,
        inactivity_timeout => 600000,
        request_timeout => 180000,
        idle_timeout => 1800000
    }),

    cr_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
