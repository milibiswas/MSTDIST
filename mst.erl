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
%  master() - Initiating process.
%  master() takes graph file names as input.
%----------------------------------------------------

master(Input_graph) ->

        % -------------------------------------
	% section : Input data from file 
	% -------------------------------------
        MPid = self(),                                         % master process PID
	{ok, [Name|WEs]} = file:consult(Input_graph),          % read undirected graph from file Name=[a,b,c,d], WEs are list of others. See graph.txt file
        io:format("~p~n",[WEs]),
        PLs = create_process(WEs,MPid),                        % PLs :: {Node,Pid}
        [Pid!{processlist,PLs}||{N,Pid}<-PLs],

        %------------------------------------------
        % This process start the wakeup
        %------------------------------------------
        %Tup = nth(1, PLs),                                     % return tuple{a,Pid}, taking 1st tuple for wakeup
        %Pid = element(2,Tup),
        [Pid!wakeup||{_,Pid}<-PLs]                             % send wakeup message to the PID
        %io:format("~p~n",[Pid])
        .



%-----------------------------------------------------
% create_process() - process creation function
% This function create individual process for
% each node.
%-----------------------------------------------------

create_process(WEs,MPid) -> [{nth(1,Vs),spawn(mst,process,[MPid,Vs])}||Vs<-WEs].

%-----------------------------------------------------
%
%    process(MPid,Vs) : This function calls process/4
%    ******* Vs = WEs *******
%-----------------------------------------------------

process(MPid,Vs) ->
      process(MPid,Vs,[],[dummy,dummy,sleeping],[],nil).
      



%------------------------------------------------------
% process() - this is the code runs in every process()
% This process() which is process/4 that remains alive
% in everz process. In other words each node remains
% active for processin till end.
%------------------------------------------------------

process(MPid,Vs,PLs,SEs,DEs,TestEdge)->
          receive


	     {processlist,PLs1}->
               		process(MPid,Vs,PLs1,SEs,[],nil);


             wakeup -> 
               		SEs_new=wakeup(Vs,PLs),               %% SEs_new = [LN,MW,SN,FC,[{node,dest node, weight, basic/branch/rejected},{...}...] ]

                        file:write_file("mst_wakeup.txt", io_lib:format("~p~n", [SEs_new]), [append]),
                	
                        process(MPid,Vs,PLs,SEs_new,[],nil);

             {connect,L,Node,SPid,MW} -> 

%%-----------------------------------------------------------------------------------
%%
%%     This below portion of code will be called when the node is in sleeping mode.
%%
%%-----------------------------------------------------------------------------------

                       A=nth(3,SEs),          % The variable A is store 3rd element of SEs which is State of the node (SN)
                       if 
                          (A == sleeping) -> 
                                      Z=wakeup(Vs,PLs),     % Since the node is sleeping, wakeup process will be called.
                                      B=nth(1,Z),           % Here variable B stores the Level Number (LN) returned by wakeup process. 
                                      file:write_file("ms_edge_update.txt", io_lib:format("~p~n", [Z]), [append]),
 				                                         

				      if 
			 		 L < B -> 
                                                     SEs_new = edge_update(nth(5,Z),Node),
                                                     SPid!{initiateMessage,nth(1,SEs_new),nth(2,SEs_new),nth(3,SEs_new),nth(1,Vs)},
                                                     S=nth(3,SEs_new),
       						     if 
                                               		  S==find -> 
                                                          FC=nth(4,SEs_new)+1,
                                                          Z=[nth(1,SEs_new),nth(2,SEs_new),nth(3,SEs_new),FC|[nth(5,SEs_new)]],
                                                          process(MPid,Vs,PLs,Z,[],nil);
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
                                                             process(MPid,Vs,PLs,SEs_new_1,[],nil);   
                                                    true -> true
					end;

					true -> true

				      end; 
%%-----------------------------------------------------------------------------------------
%%
%%     This below portion of code will be called when the node is NOT in sleeping mode.
%%
%%-----------------------------------------------------------------------------------------

                          (A /= sleeping) -> 
                                      Z=SEs,     % Since the node is sleeping, wakeup process will be called.
                                      B=nth(1,Z),           % Here variable B stores the Level Number (LN) returned by wakeup process. 
                                      file:write_file("ms_edge_update.txt", io_lib:format("~p~n", [Z]), [append]),
 				                                         

				      if 
			 		 L < B -> 
                                                     SEs_new = edge_update(nth(5,Z),Node),
                                                     SPid!{initiateMessage,nth(1,SEs_new),nth(2,SEs_new),nth(3,SEs_new),nth(1,Vs)},
                                                     S=nth(3,SEs_new),
       						     if 
                                               		  S==find -> 
                                                          FC=nth(4,SEs_new)+1,
                                                          Z=[nth(1,SEs_new),nth(2,SEs_new),nth(3,SEs_new),FC|[nth(5,SEs_new)]],
                                                          process(MPid,Vs,PLs,Z,[],nil);
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
                                                             process(MPid,Vs,PLs,SEs_new_1,[],nil);   
                                                    true -> true
					end;

					true -> true

				      end
                                    end;

%%------------------------------------------------------------------------------------------------------------------------------------------

              {initiateMessage,L,F,S,Node} ->

                               DEs_new = [Node,nil,infinit],                                                       %in-branch,best-edge,best-wt
   		               R=[B||{U,V,W,X}<-nth(5,SEs),{A,B}<-PLs, ((X==branch) and (A==V) and (A /= Node))],  % Here B is Pid
					   
			       [Pid!{initiateMessage,L,F,S,nth(1,Vs)}||Pid<-R],                                    % sendng initiate message

                               


                               if 
                                 S==find -> 
                                           FC=nth(4,SEs)+length(R),
                                           SEs_new=[L,F,S,FC|[nth(5,SEs)]];
                                               
                                 S/=find ->
                                           SEs_new=[L,F,S,nth(4,SEs)|[nth(5,SEs)]]
	
                               end,
                               
                               if
                                 S==find ->                                           
                                           TestEdge=testMessage(nth(5,SEs_new),PLs,L,F),
                                           file:write_file("ms_initiate_msg.txt", io_lib:format("~p~n~p~n", [SEs_new,TestEdge]), [append]),
                                           process(MPid,Vs,PLs,SEs_new,DEs_new,TestEdge);          % MPid,Vs,PLs,SEs,[in-branch,best-edge,best-weight],test-weight
                                 S/=find ->
                                           TestEdge=nil,
                                           process(MPid,Vs,PLs,SEs_new,DEs_new,TestEdge)           % MPid,Vs,PLs,SEs,[in-branch,best-edge,best-weight],test-weight

                               end;

					   
 {test,LN,FN,Node} ->
         V_sleep = nth(3,SEs),

         %------------------------------------------------
         % first check if the node is in sleeping state
         % then run wakeup()
         %------------------------------------------------
          
	  if 
            (V_sleep==sleeping)-> 
          
            SEs_new = wakeup(Vs,PLs),
            L=nth(1,SEs_new),
            F=nth(2,SEs_new),
             file:write_file("mst_testresp.txt", io_lib:format("~p~n", [SEs_new]), [append]),

          if
              (L=<LN) ->
                       if
                          (F/=FN) -> 
                                     [Pid!{accept,nth(1,Vs)}||{N,Pid}<-PLs, (N==Node)],    % N is node for Pid, sending accept message on edge j
                                     process(MPid,Vs,PLs,SEs_new,DEs,TestEdge);
                          (F==FN) ->  
                                     XX=nth(5,SEs_new),
                                     [{Label,Weight}]=[{X,W}||{U,V,W,X}<-XX,(V==Node),(X==basic)],
                                    if 
                                       (Label==basic) ->
						     L1=[{U,V,W,rejected}||{U,V,W,X}<-XX,(V==Node),(X==basic)],
                                                     L2=[{U,V,W,X}||{U,V,W,X}<-XX,(V/=Node)],
                                                     SEs_1=L1++L2,
                                                     if 
                                                        (Weight /= TestEdge) -> 
                                                          
                                                          [Pid!{reject,nth(1,Vs)}||{N,Pid}<-PLs, (N==Node)],
                                                          process(MPid,Vs,PLs,([nth(1,SEs_new),nth(2,SEs_new),nth(3,SEs_new),nth(4,SEs_new)]++[SEs_1]),DEs,TestEdge);
                                                         
                                                        (Weight == TestEdge) ->
                                                                    TestEdge1=testMessage(SEs_1,PLs,nth(1,SEs_new),nth(1,SEs_new)),
                                                          process(MPid,Vs,PLs,([nth(1,SEs_new),nth(2,SEs_new),nth(3,SEs_new),nth(4,SEs_new)]++[SEs_1]),DEs,TestEdge1)
                       
                                                     end;
                                                                                                                                                            
                                       (Label /= basic) ->
                                                    if 
                                                        (Weight /= TestEdge) -> 
                                                          
                                                          [Pid!{reject,nth(1,Vs)}||{N,Pid}<-PLs, (N==Node)],
                                                          process(MPid,Vs,PLs,SEs_new,DEs,TestEdge);
                                                         
                                                        (Weight == TestEdge) ->
                                                                    TestEdge1=testMessage(nth(5,SEs_new),PLs,nth(1,SEs_new),nth(1,SEs_new)),
                                                          process(MPid,Vs,PLs,SEs_new,DEs,TestEdge1)
                       
                                                     end
                                    end  
                        end;
              
              true -> 
                      true    % ToDo -- L>LN, create the message queue
          end;

       (V_sleep/=sleeping)->
            L=nth(1,SEs),
            F=nth(2,SEs),
            file:write_file("mst_testresp.txt", io_lib:format("~p~n", [SEs]), [append]),
          if
              (L=<LN) ->
                       if
                          (F/=FN) -> 
                                     [Pid!{accept,nth(1,Vs)}||{N,Pid}<-PLs, (N==Node)],    % N is node for Pid, sending accept message on edge j
                                     process(MPid,Vs,PLs,SEs,DEs,TestEdge);
                          (F==FN) ->  
                                     XX=nth(5,SEs),
                                     [{Label,Weight}]=[{X,W}||{U,V,W,X}<-XX,(V==Node),(X==basic)],
                                    if 
                                       (Label==basic) ->
						     L1=[{U,V,W,rejected}||{U,V,W,X}<-XX,(V==Node),(X==basic)],
                                                     L2=[{U,V,W,X}||{U,V,W,X}<-XX,(V/=Node)],
                                                     SEs_1=L1++L2,
                                                     if 
                                                        (Weight /= TestEdge) -> 
                                                          
                                                          [Pid!{reject,nth(1,Vs)}||{N,Pid}<-PLs, (N==Node)],
                                                          process(MPid,Vs,PLs,([nth(1,SEs),nth(2,SEs),nth(3,SEs),nth(4,SEs)]++[SEs_1]),DEs,TestEdge);
                                                         
                                                        (Weight == TestEdge) ->
                                                                    TestEdge1=testMessage(SEs_1,PLs,nth(1,SEs),nth(1,SEs)),
                                                          process(MPid,Vs,PLs,([nth(1,SEs),nth(2,SEs),nth(3,SEs),nth(4,SEs)]++[SEs_1]),DEs,TestEdge1)
                       
                                                     end;
                                                                                                                                                            
                                       (Label /= basic) ->
                                                    if 
                                                        (Weight /= TestEdge) -> 
                                                          
                                                          [Pid!{reject,nth(1,Vs)}||{N,Pid}<-PLs, (N==Node)],
                                                          process(MPid,Vs,PLs,SEs,DEs,TestEdge);
                                                         
                                                        (Weight == TestEdge) ->
                                                                    TestEdge1=testMessage(nth(5,SEs),PLs,nth(1,SEs),nth(1,SEs)),
                                                          process(MPid,Vs,PLs,SEs,DEs,TestEdge1)
                       
                                                     end
                                    end  
                        end;
              
              true -> 
                      true    % ToDo -- L>LN, create the message queue
          end

       end;

{accept,LN,FN,Node} ->
       TestEdge1=nil,
       [Wj]=[W||{U,V,W,X}<-nth(5,SEs),(V==Node)],
       [BestWeight]=[W1||{_,_,W1}<-nth(6,SEs)],

       if
           (Wj<BestWeight) ->
                             BEdge=Node,
                             BWeight=Wj,
                             DEs_new=[Node,BEdge,BWeight],
                             %ToDo execute report procedure
                             process(MPid,Vs,PLs,SEs,DEs_new,TestEdge1);
           (Wj>=BestWeight) ->
                            %ToDo execute report procedure
                            process(MPid,Vs,PLs,SEs,DEs,TestEdge)

       end;

{reject,LN,FN,Node} ->
          ok

                                          
    end.


%%-------------------------------------------------------------------------------------
%%
%%  Here below all the helper functions that are called from above portion of code
%%
%%-------------------------------------------------------------------------------------

%----------------------
% testMessage() 
%----------------------

testMessage(T,PLs,LN,FN)->

              Tmp_lst=[W||{U,V,W,X}<-T,(X==basic)],   % Create temporarz list with adj edge basic
              if 
                length(Tmp_lst)>0 ->
                		MW=min([W||{U,V,W,X}<-T,(X==basic)]),                 % Here find minimum weight of adjacent node with label basic
                		[{Src_Node,Node}]=[{U,V}||{U,V,W,X}<-T,(W==MW)],      % Destination with min weight edge
                		[B!{test,LN,FN,Src_Node}||{A,B}<-PLs,(A==Node)],
                                MW;                                                   % MW will be returned to be assigned to test-edge
                length(Tmp_lst)=<0 ->
                      reportMessage,            % ToDo -- here start report.
                      nil                       % nil will be returned to be assigned to test-edge


              end.
              


                 

%--------------------------------------
%   acceptMessage()
%--------------------------------------

%accpetMessage(LN,FN,Vs,PLs,SEs,Node,BestEdge) ->
 %        TestEdge=nil,
  %       [Wj]=[X||{U,V,W,X}<-nth(5,SEs)],
   %      if
           
      

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
