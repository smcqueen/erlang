%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosNotifyComm_StructuredPushConsumer
%% Source: /net/isildur/ldisk/daily_build/otp_prebuild_r14a.2010-06-15_18/otp_src_R14A/lib/cosNotification/src/CosNotifyComm.idl
%% IC vsn: 4.2.25
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosNotifyComm_StructuredPushConsumer').
-ic_compiled("4_2_25").


%% Interface functions
-export([push_structured_event/2, push_structured_event/3, disconnect_structured_push_consumer/1]).
-export([disconnect_structured_push_consumer/2]).

%% Exports from "CosNotifyComm::NotifyPublish"
-export([offer_change/3, offer_change/4]).

%% Type identification function
-export([typeID/0]).

%% Used to start server
-export([oe_create/0, oe_create_link/0, oe_create/1]).
-export([oe_create_link/1, oe_create/2, oe_create_link/2]).

%% TypeCode Functions and inheritance
-export([oe_tc/1, oe_is_a/1, oe_get_interface/0]).

%% gen server export stuff
-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3]).
-export([handle_cast/2, handle_info/2, code_change/3]).

-include_lib("orber/include/corba.hrl").


%%------------------------------------------------------------
%%
%% Object interface functions.
%%
%%------------------------------------------------------------



%%%% Operation: push_structured_event
%% 
%%   Returns: RetVal
%%   Raises:  CosEventComm::Disconnected
%%
push_structured_event(OE_THIS, Notification) ->
    corba:call(OE_THIS, push_structured_event, [Notification], ?MODULE).

push_structured_event(OE_THIS, OE_Options, Notification) ->
    corba:call(OE_THIS, push_structured_event, [Notification], ?MODULE, OE_Options).

%%%% Operation: disconnect_structured_push_consumer
%% 
%%   Returns: RetVal
%%
disconnect_structured_push_consumer(OE_THIS) ->
    corba:call(OE_THIS, disconnect_structured_push_consumer, [], ?MODULE).

disconnect_structured_push_consumer(OE_THIS, OE_Options) ->
    corba:call(OE_THIS, disconnect_structured_push_consumer, [], ?MODULE, OE_Options).

%%%% Operation: offer_change
%% 
%%   Returns: RetVal
%%   Raises:  CosNotifyComm::InvalidEventType
%%
offer_change(OE_THIS, Added, Removed) ->
    corba:call(OE_THIS, offer_change, [Added, Removed], ?MODULE).

offer_change(OE_THIS, OE_Options, Added, Removed) ->
    corba:call(OE_THIS, offer_change, [Added, Removed], ?MODULE, OE_Options).

%%------------------------------------------------------------
%%
%% Inherited Interfaces
%%
%%------------------------------------------------------------
oe_is_a("IDL:omg.org/CosNotifyComm/StructuredPushConsumer:1.0") -> true;
oe_is_a("IDL:omg.org/CosNotifyComm/NotifyPublish:1.0") -> true;
oe_is_a(_) -> false.

%%------------------------------------------------------------
%%
%% Interface TypeCode
%%
%%------------------------------------------------------------
oe_tc(push_structured_event) -> 
	{tk_void,
            [{tk_struct,"IDL:omg.org/CosNotification/StructuredEvent:1.0",
                 "StructuredEvent",
                 [{"header",
                   {tk_struct,"IDL:omg.org/CosNotification/EventHeader:1.0",
                       "EventHeader",
                       [{"fixed_header",
                         {tk_struct,
                             "IDL:omg.org/CosNotification/FixedEventHeader:1.0",
                             "FixedEventHeader",
                             [{"event_type",
                               {tk_struct,
                                   "IDL:omg.org/CosNotification/EventType:1.0",
                                   "EventType",
                                   [{"domain_name",{tk_string,0}},
                                    {"type_name",{tk_string,0}}]}},
                              {"event_name",{tk_string,0}}]}},
                        {"variable_header",
                         {tk_sequence,
                             {tk_struct,
                                 "IDL:omg.org/CosNotification/Property:1.0",
                                 "Property",
                                 [{"name",{tk_string,0}},{"value",tk_any}]},
                             0}}]}},
                  {"filterable_data",
                   {tk_sequence,
                       {tk_struct,"IDL:omg.org/CosNotification/Property:1.0",
                           "Property",
                           [{"name",{tk_string,0}},{"value",tk_any}]},
                       0}},
                  {"remainder_of_body",tk_any}]}],
            []};
oe_tc(disconnect_structured_push_consumer) -> 
	{tk_void,[],[]};
oe_tc(offer_change) -> 'CosNotifyComm_NotifyPublish':oe_tc(offer_change);
oe_tc(_) -> undefined.

oe_get_interface() -> 
	[{"offer_change", 'CosNotifyComm_NotifyPublish':oe_tc(offer_change)},
	{"disconnect_structured_push_consumer", oe_tc(disconnect_structured_push_consumer)},
	{"push_structured_event", oe_tc(push_structured_event)}].




%%------------------------------------------------------------
%%
%% Object server implementation.
%%
%%------------------------------------------------------------


%%------------------------------------------------------------
%%
%% Function for fetching the interface type ID.
%%
%%------------------------------------------------------------

typeID() ->
    "IDL:omg.org/CosNotifyComm/StructuredPushConsumer:1.0".


%%------------------------------------------------------------
%%
%% Object creation functions.
%%
%%------------------------------------------------------------

oe_create() ->
    corba:create(?MODULE, "IDL:omg.org/CosNotifyComm/StructuredPushConsumer:1.0").

oe_create_link() ->
    corba:create_link(?MODULE, "IDL:omg.org/CosNotifyComm/StructuredPushConsumer:1.0").

oe_create(Env) ->
    corba:create(?MODULE, "IDL:omg.org/CosNotifyComm/StructuredPushConsumer:1.0", Env).

oe_create_link(Env) ->
    corba:create_link(?MODULE, "IDL:omg.org/CosNotifyComm/StructuredPushConsumer:1.0", Env).

oe_create(Env, RegName) ->
    corba:create(?MODULE, "IDL:omg.org/CosNotifyComm/StructuredPushConsumer:1.0", Env, RegName).

oe_create_link(Env, RegName) ->
    corba:create_link(?MODULE, "IDL:omg.org/CosNotifyComm/StructuredPushConsumer:1.0", Env, RegName).

%%------------------------------------------------------------
%%
%% Init & terminate functions.
%%
%%------------------------------------------------------------

init(Env) ->
%% Call to implementation init
    corba:handle_init('CosNotifyComm_StructuredPushConsumer_impl', Env).

terminate(Reason, State) ->
    corba:handle_terminate('CosNotifyComm_StructuredPushConsumer_impl', Reason, State).


%%%% Operation: push_structured_event
%% 
%%   Returns: RetVal
%%   Raises:  CosEventComm::Disconnected
%%
handle_call({_, OE_Context, push_structured_event, [Notification]}, _, OE_State) ->
  corba:handle_call('CosNotifyComm_StructuredPushConsumer_impl', push_structured_event, [Notification], OE_State, OE_Context, false, false);

%%%% Operation: disconnect_structured_push_consumer
%% 
%%   Returns: RetVal
%%
handle_call({_, OE_Context, disconnect_structured_push_consumer, []}, _, OE_State) ->
  corba:handle_call('CosNotifyComm_StructuredPushConsumer_impl', disconnect_structured_push_consumer, [], OE_State, OE_Context, false, false);

%%%% Operation: offer_change
%% 
%%   Returns: RetVal
%%   Raises:  CosNotifyComm::InvalidEventType
%%
handle_call({_, OE_Context, offer_change, [Added, Removed]}, _, OE_State) ->
  corba:handle_call('CosNotifyComm_StructuredPushConsumer_impl', offer_change, [Added, Removed], OE_State, OE_Context, false, false);



%%%% Standard gen_server call handle
%%
handle_call(stop, _, State) ->
    {stop, normal, ok, State};

handle_call(_, _, State) ->
    {reply, catch corba:raise(#'BAD_OPERATION'{minor=1163001857, completion_status='COMPLETED_NO'}), State}.


%%%% Standard gen_server cast handle
%%
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply, State}.


%%%% Standard gen_server handles
%%
handle_info(_, State) ->
    {noreply, State}.


code_change(OldVsn, State, Extra) ->
    corba:handle_code_change('CosNotifyComm_StructuredPushConsumer_impl', OldVsn, State, Extra).

