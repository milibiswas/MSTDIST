%----------------------------------------------------------------------------
% GHS algorithm
% Context: Minimum Spanning Tree(MST)

% Compile with: c(mst).
% Examples:
%    mst:master("graph.txt").

% Ref: GHS.
% Authors: Mili Biswas, Maxence Feller, University of Fribourg, Switzerland; 
%----------------------------------------------------------------------------

-module(mst).
-import(lists,[nth/2,min/1]). 
-export([master/1,process/2]).

%----------------------------------------------------
%  master() - Initiating process
%----------------------------------------------------

master(Input_graph) ->

        % -------------------------------------
	% section : Input data from file 
	% -------------------------------------
        MPid = self(),
	{ok, [Name|WEs]} = file:consult(Input_graph),          % read undirected graph from file
        io:format("~p~n",[WEs]),
        PLs = create_process(WEs,MPid),                        % PLs :: {Node,Pid}
        [Pid!{processlist,PLs}||{N,Pid}<-PLs],
        Tup = nth(1, PLs),  % return tuple{a,Pid}              % Picking a process for first wakeup
        Pid = element(2,Tup),
        [Pid!wakeup||{_,Pid}<-PLs],
        io:format("~p~n",[Pid]).



%-----------------------------------------------------
% create_process() - process creation function
%-----------------------------------------------------

create_process(Vertex_set,MPid) -> [{nth(1,Vs),spawn(mst,process,[MPid,Vs])}||Vs<-Vertex_set].


process(MPid,Vs) ->
      process(MPid,Vs,[],[dummy,dummy,sleeping]).
      



%------------------------------------------------------
% process() - this is the code runs in every process()
%------------------------------------------------------

process(MPid,Vs,PLs,SEs)->
          receive
             wakeup -> 
               		SEs_new=wakeup(Vs,PLs),
                        file:write_file("mst_wakeup.txt", io_lib:format("~p~n", [SEs_new]), [append]),
                	process(MPid,Vs,PLs,SEs_new);

	     {processlist,PLs1}->
               		process(MPid,Vs,PLs1,SEs);
             {connect,L,Node,SPid,MW} -> 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%     This below portion of code will be called when the node is in sleeping mode.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                       A=nth(3,SEs),          % The variable A is store 3rd element of SEs which is State of the node (SN)
                       if 
                          (A == sleeping) -> 
                                      Z=wakeup(Vs,PLs),     % Since the node is sleeping, wakeup process will be called.
                                      B=nth(1,Z),           % Here variable B stores the Level Number (LN) returned by wakeup process. 
                                      %file:write_file("ms_edge_update.txt", io_lib:format("~p~n", [Z]), [append]),
 				                                         

				      if 
			 		 L < B -> 
                                                     SEs_new = edge_update(nth(5,Z),Node),
                                                     SPid!{initiateMessage,nth(1,SEs_new),nth(2,SEs_new),nth(3,SEs_new),nth(1,Vs)},
                                                     S=nth(3,SEs_new),
       						     if 
                                               		  S==find -> 
                                                          FC=nth(4,SEs_new)+1,
                                                          Z=[nth(1,SEs_new),nth(2,SEs_new),nth(3,SEs_new),FC|[nth(5,SEs_new)]],
                                                          process(MPid,Vs,PLs,Z);
                                               		  true -> true
                                                     end;
                          		 true ->true 
                        	      end,
				      if
					L>=B -> 
                                                 L1 = nth(5,Z),
                                                 R=[1||{U,V,W,X}<-L1,(V==Node) and (X==basic)],   % Here R is for checking if the edge is branch or basic
                                                 if
                                                    R==[1] -> messageQue;  
							      % ToDo how to put the request on queue   
                                                    true -> true
					         end,
                                                 if
                                                    R==[] -> SPid!{initiateMessage,nth(1,Z)+1,MW,find,nth(1,Vs)},
                                                             SEs_new_1 = [nth(1,Z)+1,MW,find,nth(1,Z)|[nth(5,Z)]],
                                                             process(MPid,Vs,PLs,SEs_new_1);   
                                                    true -> true
					end;

					true -> true

				      end; 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%     This below portion of code will be called when the node is NOT in sleeping mode.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                          (A /= sleeping) -> 
                                      Z=SEs,     % Since the node is sleeping, wakeup process will be called.
                                      B=nth(1,Z),           % Here variable B stores the Level Number (LN) returned by wakeup process. 
                                      %file:write_file("ms_edge_update.txt", io_lib:format("~p~n", [Z]), [append]),
 				                                         

				      if 
			 		 L < B -> 
                                                     SEs_new = edge_update(nth(5,Z),Node),
                                                     SPid!{initiateMessage,nth(1,SEs_new),nth(2,SEs_new),nth(3,SEs_new),nth(1,Vs)},
                                                     S=nth(3,SEs_new),
       						     if 
                                               		  S==find -> 
                                                          FC=nth(4,SEs_new)+1,
                                                          Z=[nth(1,SEs_new),nth(2,SEs_new),nth(3,SEs_new),FC|[nth(5,SEs_new)]],
                                                          process(MPid,Vs,PLs,Z);
                                               		  true -> true
                                                     end;
                          		 true ->true 
                        	      end,
				      if
					L>=B -> 
                                                 L1 = nth(5,Z),
                                                 R=[1||{U,V,W,X}<-L1,(V==Node) and (X==basic)],   % Here R is for checking if the edge is branch or basic
                                                 if
                                                    R==[1] -> messageQue;  
							      % ToDo how to put the request on queue   
                                                    true -> true
					         end,
                                                 if
                                                    R==[] -> SPid!{initiateMessage,nth(1,Z)+1,MW,find,nth(1,Vs)},
                                                             SEs_new_1 = [nth(1,Z)+1,MW,find,nth(1,Z)|[nth(5,Z)]],
                                                             process(MPid,Vs,PLs,SEs_new_1);   
                                                    true -> true
					end;

					true -> true

				      end
                       end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

              {initiateMessage,L,F,S,Node} ->
                                           DEs_new = [Node,nil,infinit],
   					   R=[B||{U,V,W,X}<-nth(5,SEs),{A,B}<-PLs, ((X==branch) and (A==V) and (A /= Node))],
                                           if 
                                               S==find -> 
                                                          FC=nth(4,SEs)+1,
                                                          SEs_new=[L,F,S,FC|[nth(5,SEs)]],
                                                          initiateTest; %ToDo write procedure to initiate test%
                                               true -> true
                                           end,
                                           file:write_file("ms_output.txt", io_lib:format("~p~n~p~n", [SEs,R]), [append])
                  
           end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Here below all the helper functions that are called from above portion of code
%%
%%

%------------------------------------------------------
% wakeup() function implements wakeup process
%------------------------------------------------------

wakeup(V,CPs)->

        LN=0,      % define Level Number (LN)
        SN=found,  % define State of Node (SN)
        FC=0,      % define find count varaible (FC)

        [Name|Rest]= V,
        SEs = [{Name,Node,Weight,basic}||{Node,Weight}<-Rest], 

       %--------------------------------------------------------------------
       % Calculate the minimum adjacent outgoing edge of node 
       % Here returned Pid is the other end node Pid of Mwoe (Core node Pid)
       %--------------------------------------------------------------------

	{Mwoe,Pid} = find_min_weighted_edge(V,CPs), 

       %------------------------------------------------------
       % Sending connect message
       %------------------------------------------------------

	Pid!{connect,LN,Name,self(),Mwoe},
       
       %------------------------------------------------------------------------
       % List of adjacent edges preparation (Making min weighted edge as branch)
       %------------------------------------------------------------------------
        Node1=find_node_from_weight(Mwoe,Rest),
        [SEs_1]=[{U,V,W,branch}||{U,V,W,X}<-SEs, (V==Node1)], 
        SEs_2=[{U,V,W,basic}||{U,V,W,X}<-SEs, (V/=Node1)],
        [LN,Mwoe,SN,FC]++[[SEs_1|SEs_2]].   % Return the common data structure of each node
         
%----------------------------------------------------------------------------------
% find_min_weighted_edge() function returns tuple {MinWeight,Pid of other end node}
%-----------------------------------------------------------------------------------

find_min_weighted_edge([H|T],Created_process)->
			    Mw=find_min_weight(T),
                            Pid=find_process_id(Mw,T,Created_process),
                            {Mw,Pid}.

%-----------------------------------------------------------------------------------
% find_min_weight() function calculates minnimum value of lists of weights
%-----------------------------------------------------------------------------------

find_min_weight(T)->
	min([V||{U,V}<-T]).


%--------------------------------------------------------------------------
% find_process_id() function return pid of Minimum Weight's other end node
%--------------------------------------------------------------------------

find_process_id(Mw,T,Created_process)->
                Node=find_node_from_weight(Mw,T),
                [Pid]=[V||{U,V}<-Created_process,(U==Node)],
                 Pid.

find_node_from_weight(Mw,T)->
         [Node]=[U||{U,V}<-T,(V==Mw)],
          Node.
edge_update(Ls,Node) ->
     
            [SEs_1]=[{U,V,W,branch}||{U,V,W,X}<-Ls, (V==Node)],
            SEs_2=[{U,V,W,basic}||{U,V,W,X}<-Ls, (V/=Node)],
            [SEs_1|SEs_2].
