%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%%  This library provides comprehensive validation for the different
%%%  Erlang/OTP package types.
%%% @end
%%%-------------------------------------------------------------------
-module(ewl_package_info).

%% API
-export([
	 package_type/1,
	 app_type/1,
	 verify_presence_of_erl_files/1,
	 verify_app_erts_vsn/1,
	 assert_erts/1,
	 assert_application/1,
	 assert_binary_application/1,
	 assert_unbuilt_application/1,
	 assert_release/1,
	 is_valid_control_file/1,
	 is_valid_signature_file/1
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Determine the type of the package and make sure it is a valid instance of that type.
%% @spec validate_type(PackageDir) -> {ok, Type} | {error, Reason}
%% where
%%  Type = application | release | erts
%% @end
%%--------------------------------------------------------------------
package_type(PackageDir) ->
    ?INFO_MSG("is it erts, binary, or generic ~p~n", [PackageDir]),
    case catch assert_erts(PackageDir) of
	true ->
	    {ok, erts};
	{'EXIT', ReasonE} -> 
	    ?INFO_MSG("~p~n", [ReasonE]),
	    case catch assert_application(PackageDir) of
		true ->
		    {ok, application};
		{'EXIT', ReasonA} ->
		    ?INFO_MSG("~p~n", [ReasonA]),
		    case catch assert_release(PackageDir) of
			true  ->
			    {ok, release};
			{'EXIT', ReasonR} ->
			    ?INFO_MSG("~p~n", [ReasonA]),
			    {error, badly_formatted_package}
		    end
	    end
    end.

%%--------------------------------------------------------------------
%% @doc Determine the type of the application making sure it is a
%%  valid instance of that type.
%% <ul>
%%  <el>unbuilt - there is no object code present in the app</el>
%%  <el>binary - contains non-erlang object code</el>
%%  <el>generic - contains object code but only *.beam object code</el>
%% </ul>
%% @spec validate_type(PackageDir) -> {ok, Type} | {error, Reason}
%% where
%%  Type = unbuilt | binary | generic
%% @end
%%--------------------------------------------------------------------
app_type(PackageDir) ->
    case catch assert_application(PackageDir) of
	true ->
	    case catch assert_unbuilt_application(PackageDir) of
		true ->
		    {ok, unbuilt};
		{'EXIT', ReasonU} ->
		    ?INFO_MSG("~p~n", [ReasonA]),
		    case assert_binary_application(PackageDir) of
			true  -> {ok, binary};
			false -> {ok, generic}
		    end
	    end;
	{'EXIT', Reason} ->
	    ?INFO_MSG("~p~n", [ReasonA]),
	    {error, badly_formatted_application}
    end.
	    

%%--------------------------------------------------------------------
%% @doc Determine if the supplied package is an ERTS package.
%% @spec assert_erts(PackageDir) -> true | exit()
%% @end
%%--------------------------------------------------------------------
assert_erts(PackageDir) ->
    lists:all(fun(F) -> F(PackageDir) end, [
	
	%% Run all the following lambda's and if all of them return true then we have a well formed application.
	
	fun(PackageDir_) ->  
            case filelib:wildcard(PackageDir_ ++ "/include/driver_int.h") of
		[_|_] -> 
		    true;
		[] -> 
		    false
	    end
	end, 

	fun(PackageDir_) ->  
            case filelib:wildcard(PackageDir_ ++ "/include/erl_fixed_size_int_types.h") of
		[_|_] -> 
		    true;
		[] -> 
		    false
	    end
	end 
    ]).

%%--------------------------------------------------------------------
%% @doc Determine if the supplied package is an application package.
%% @spec assert_application(PackageDir) -> true | exit()
%% @end
%%--------------------------------------------------------------------
assert_application(PackageDir) ->
    lists:all(fun(F) -> F(PackageDir) end, [
	
	%% Run all the following lambda's and if all of them return true then we have a well formed application.
	
	fun(PackageDir_) ->  
            case filelib:wildcard(PackageDir_ ++ "/ebin/*.app") of
		[_|_] -> 
		    true;
		[] -> 
		    throw(app_file_not_found)
	    end
	end, 
	
	fun(PackageDir_) ->
            case verify_presence_of_erl_files(PackageDir_) of
		ok -> 
                    true;
		{error, Reason} -> 
                    throw(Reason)
	    end
	end
    ]).

%%--------------------------------------------------------------------
%% @doc Determine if the supplied package is an application package.
%% @spec assert_unbuilt_application(PackageDir) -> true | exit()
%% @end
%%--------------------------------------------------------------------
assert_unbuilt_application(PackageDir) ->
    lists:all(fun(F) -> F(PackageDir) end, [
	
	%% Run all the following lambda's and if all of them return true the package dir is an unbuilt app and the function
	%% will return true.
	
	fun(PackageDir_) ->  
		case 0 == length(filelib:wildcard(PackageDir_ ++ "/ebin/*beam")) of
		    true  -> true;
		    false -> throw(app_contains_beams)
		end
	end 
	
        %fun(PackageDir_) ->
                %Files = [filename:basename(F) || F <- ewl_file:find(PackageDir_, ".*")],
		%% For now only a build.sh file constitutes unbuilt. Perhaps configure and make files in the future.
                %lists:any(fun(File) -> lists:member(File, Files) end, ["build.sh"])
        %end
    ]).

%%--------------------------------------------------------------------
%% @doc Determine if the supplied package is a binary application package.
%% @spec assert_binary_application(PackageDir) -> true | exit()
%% @end
%%--------------------------------------------------------------------
assert_binary_application(PackageDir) ->
    lists:any(fun(F) -> F(PackageDir) end, [
	
	%% Run all the following lambda's and if any of them return true the package dir is a binary app and the function
	%% will return true.
	
	fun(PackageDir_) ->  
	    lists:any(fun(Dir) -> 
		case re:run(Dir, ".*_src") of
			{match, _} -> true;
			_          -> false
		end
	    end, filelib:wildcard(PackageDir_ ++ "/*"))
	end, 
	
	fun(PackageDir_) ->
		RegexpBody = string:strip(lists:flatten([".*\\." ++ Ext ++ "$|" || Ext <- ?BINARY_FILE_EXTENSIONS]), right, $|), 
		Exts = lists:flatten(["(", RegexpBody, ")"]),
		case ewl_file:find(PackageDir_, Exts) of
		    []    -> false;
		    [_|_] -> true
		end
	end,

        fun(PackageDir_) ->
                Files = ewl_file:find(PackageDir_, ".*"),
                lists:any(fun is_binary_file/1, Files)
        end,

        fun(PackageDir_) ->
                has_binary_override_entry(PackageDir_)
        end
    ]).
		
%%--------------------------------------------------------------------
%% @doc Determine if the supplied package is a release package.
%% @spec assert_release(PackageDir) -> true | exit()
%% @end
%%--------------------------------------------------------------------
assert_release(PackageDir) ->
    lists:all(fun(F) -> F(PackageDir) end, 
	      [
	       
	       %% Run all the following lambda's and if all of them return true then we have a well formed release.
	       
	       fun(PackageDir_) ->
		       case filelib:wildcard(PackageDir_ ++ "/releases/*/*.rel") of
			   []  -> throw(no_dot_rel_file);
			   [_] -> true
		       end
	       end
	      ]).

%%--------------------------------------------------------------------
%% @doc determine if a signature file supplied is a valid one.
%% @spec is_valid_signature_file(SignatureFilePath) -> bool()
%% @end
%%--------------------------------------------------------------------
is_valid_signature_file(SignatureFilePath) ->
    ?INFO_MSG("~p~n", [SignatureFilePath]),
    case file:consult(SignatureFilePath) of
	{ok, [Signature]} ->
	    is_valid_signature_term(Signature);
	_Error ->
	    ?ERROR_MSG("bad signature file~n", []),
	    false
    end.

is_valid_signature_term({signature, _Signature, _Modulus, _Exponent}) ->
    true;
is_valid_signature_term(_BadSigature) ->
    false.


%%--------------------------------------------------------------------
%% @doc determine if a control file supplied is a valid one.
%% @spec is_valid_control_file(ControlFilePath) -> true | {error, Reason}
%% where
%%  Reason = {missing_control_keys, {need, list()}, {found, list()}} | {bad_categories, list()} | no_categories | term()
%% @end
%%--------------------------------------------------------------------
is_valid_control_file(ControlFilePath) ->
    ?INFO_MSG("~p~n", [ControlFilePath]),
    case epkg_util:consult_control_file(?MANDITORY_CONTROL_KEYS, ControlFilePath) of
	{error, Reason} -> 
	    {error, Reason};
	Keys when length(Keys) == length(?MANDITORY_CONTROL_KEYS) -> 
	    has_bad_control_categories(ControlFilePath);
	Keys ->
	    {error, {missing_control_keys, {need, ?MANDITORY_CONTROL_KEYS}, {found, Keys}}}
    end.

has_bad_control_categories(ControlFilePath) ->
    case epkg_util:consult_control_file(categories, ControlFilePath) of
	Categories when is_list(Categories), Categories /= [] ->
	    case epkg_util:find_bad_control_categories(Categories) of
		[] -> 
		    true;
		BadCategories -> 
		    ?ERROR_MSG("bad control categories ~p~n", [BadCategories]),
		    {error, {bad_categories, BadCategories}}
	    end;
	_Error ->
	    {error, no_categories}
    end.
	    

%%--------------------------------------------------------------------
%% @doc Make sure an application contains all the source files that the .app files suggests it does.
%% @spec verify_presence_of_erl_files(AppDirPath::string()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
verify_presence_of_erl_files(AppDirPath) ->
    {ok, [{modules, Modules}]} = 
	ewr_util:fetch_local_appfile_key_values(AppDirPath, [modules]),
    F = fun(Mod) -> 
		case filelib:is_file(AppDirPath ++ "/src/" ++ atom_to_list(Mod) ++ ".erl") of
		    true -> 
			true;
		    false ->
			?INFO_MSG("~p is missing~n", [Mod]),
			false
		end
	end, 
    case catch lists:foreach(F, Modules) of
	ok     -> ok;
	Error -> {error, {"missing source files", Error}}
    end.

%%--------------------------------------------------------------------
%% @doc Verify that all beams within an application were compiled 
%%      for the same erts vsn and return that version. 
%% @spec verify_app_erts_vsn(AppDirPath) -> bool()
%% @end
%%--------------------------------------------------------------------
verify_app_erts_vsn(AppDirPath) ->
    case epkg_util:discover_app_erts_vsns(AppDirPath) of
	{ok, [_ErtsVsns]} -> true;
	{ok, _ErtsVsns}   -> false;
	Error             -> throw(Error)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% Predicate that uses the O/S supplied "file" command to determine if a given filename is an
%% executable
is_binary_file(Filename) ->
    FileType = os:cmd(io_lib:format("file -b ~s", [Filename])),
    lists:any(fun(Regex) ->
                      case re:run(FileType, Regex) of
                          {match, _} -> true;
                          _NoMatch   -> false
                      end
              end, ?BINARY_FILE_REGEX).
                              

%% Predicate that checks the application file for an override flag which will force the
%% app to be published as a "binary" app
has_binary_override_entry(PackageDir) ->
    case filelib:wildcard(PackageDir ++ "/ebin/*.app") of
        [File] ->
            case file:consult(File) of
                {ok, [{application, _, Keys}]} ->
                    proplists:get_bool(force_binary_app, Keys);
                _Other ->
                    false
            end;
        _ ->
            false
    end.
