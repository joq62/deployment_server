%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_deployment).
  
-include("deployment.hrl").

 
%% API
-export([
	 init/2,
	 update/2,
	 timer_to_call_update/1
	]).

-export([
	 get_applications_to_deploy/1
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_applications_to_deploy(RepoDir)->
    Result=case git_handler:all_filenames(RepoDir) of
	       {ok,AllFileNames}->
		   get_applications_to_deploy(AllFileNames,RepoDir,[]);
	       Error ->
		    Error
	   end,   
    Result.
	
get_applications_to_deploy([],_,Acc)->
    {ok,MyHostName}=net:gethostname(),
    [{ApplicationFile,HostName}||{ApplicationFile,HostName}<-Acc,
				 HostName=:=MyHostName];
get_applications_to_deploy([FileName|T],RepoDir,Acc)->
    {ok,[Info]}=git_handler:read_file(RepoDir,FileName), 
    DeploymentTerms=maps:get(deployments,Info),
    get_applications_to_deploy(T,RepoDir,lists:append(DeploymentTerms,Acc)).
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
timer_to_call_update(Interval)->
    timer:sleep(Interval),
    rpc:cast(node(),deployment_server,check_update_repo,[]).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%-------------------------------------------------------------------
update(RepoDir,GitPath)->
    Result=case git_handler:is_repo_updated(RepoDir) of
	       {error,["RepoDir doesnt exists, need to clone"]}->
		   ok=git_handler:clone(RepoDir,GitPath),
		   {ok,"Cloned the repo"};
	       false ->
		   ok=git_handler:update_repo(RepoDir),
		   {ok,"Pulled a new update of the repo"};
	       true ->
		   {ok,"Repo is up to date"}
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
init(RepoDir,GitPath)->
    case git_handler:is_repo_updated(RepoDir) of
	{error,["RepoDir doesnt exists, need to clone"]}->
	    ok=git_handler:clone(RepoDir,GitPath);
	false ->
	    ok=git_handler:update_repo(RepoDir);
	true ->
	    ok
    end,
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
