%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%

-module(ex_splitterWindow).

-behaviour(wx_object).

%% Client API
-export([start/1]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_event/2]).

-include_lib("wx/include/wx.hrl").

-record(state, 
	{
	  parent,
	  config
	}).

start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
    wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
    Parent = proplists:get_value(parent, Config),  
    Panel = wxPanel:new(Parent, [{size, {100, 100}}]),

    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				 [{label, "wxSplitterWindow"}]),

    Splitter = wxSplitterWindow:new(Panel, []),

    Win1 = wxTextCtrl:new(Splitter, 1, [{value, "Splitted Window 1"},
        			       {style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
    Win2 = wxTextCtrl:new(Splitter, 1, [{value, "Splitted Window 1"},
					{style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),

    wxSplitterWindow:splitVertically(Splitter, Win1, Win2),
    wxSplitterWindow:setSashGravity(Splitter,   0.5),
    %% Set pane-size =/= 0 to not unsplit on doubleclick
    %% on the splitter
    wxSplitterWindow:setMinimumPaneSize(Splitter,50),
    
    %% Add to sizers
    wxSizer:add(Sizer, Splitter, [{flag, ?wxEXPAND},
				  {proportion, 1}]),

    wxSizer:add(MainSizer, Sizer, [{proportion, 1},
				   {flag, ?wxEXPAND}]),
    wxPanel:connect(Panel, command_splitter_sash_pos_changed),
    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxSplitter{type = command_splitter_sash_pos_changed}},
	     State = #state{}) ->
    demo:format(State#state.config, "Splitter pos changed.\n", []),
    {noreply, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
    demo:format(State#state.config, "Got Info ~p\n", [Msg]),
    {noreply, State}.

handle_call(Msg, _From, State) ->
    demo:format(State#state.config, "Got Call ~p\n", [Msg]),
    {reply,{error, nyi}, State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

