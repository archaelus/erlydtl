%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc DNS name utility functions
%% @end
%%%-------------------------------------------------------------------
-module(erlydtl.forms.domain).

-import(lists).

%% API
-export([sanitise/1]).

%%====================================================================
%% API
%%====================================================================

sanitise(Domain) ->
    lists:filter(fun (C) when $A =< C, C =< $Z;
                              $a =< C, C =< $z;
                              $0 =< C, C =< $9;
                              C =:= $_;
                              C =:= $.;
                              C =:= $- ->
                         true;
                     (_) -> false
                 end,
                 Domain).


%%====================================================================
%% Internal functions
%%====================================================================
