%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosEventDomainAdmin_DiamondCreationForbidden
%% Source: /net/isildur/ldisk/daily_build/otp_prebuild_r14a.2010-06-15_18/otp_src_R14A/lib/cosEventDomain/src/CosEventDomainAdmin.idl
%% IC vsn: 4.2.25
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosEventDomainAdmin_DiamondCreationForbidden').
-ic_compiled("4_2_25").


-include("CosEventDomainAdmin.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_except,"IDL:omg.org/CosEventDomainAdmin/DiamondCreationForbidden:1.0",
                   "DiamondCreationForbidden",
                   [{"diam",{tk_sequence,{tk_sequence,tk_long,0},0}}]}.

%% returns id
id() -> "IDL:omg.org/CosEventDomainAdmin/DiamondCreationForbidden:1.0".

%% returns name
name() -> "CosEventDomainAdmin_DiamondCreationForbidden".



