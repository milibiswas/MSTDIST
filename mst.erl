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
-export([master/1,process/3]).

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

        %Message Queue
       
        MsgQueue=queue:new(),         % This is empty message initially

        PLs = create_process(WEs,MPid,MsgQueue),                        % PLs :: {Node,Pid}
        [Pid!{processlist,PLs}||{N,Pid}<-PLs],

        %------------------------------------------
        % This process start the wakeup
        %------------------------------------------
        Tup = nth(1, PLs),                                     % return tuple{a,Pid}, taking 1st tuple for wakeup
        Pid = element(2,Tup),
        [Pid!wakeup||{_,Pid}<-PLs],                             % send wakeup message to the PID
        %io:format("~p~n",[Pid])
        outputData([]).

       outputData(DD)->
       receive
               {complete,A,B} ->
                       %file:write_file("output_1.txt", io_lib:format("~p~n", [B]), [append]),
                       X=DD++B, 
                       outputData(X);	
                {halt,X} ->
                       file:write_file("output_halt.txt", io_lib:format("~p~n", [X]), [append]),
                       outputData(DD)
                 after 2000 ->
                     Set = sets:from_list(DD),
                     D_=sets:to_list(Set),
                     file:write_file("output.txt", io_lib:format("~p~n", [D_]), [append])                        
       end.


%-----------------------------------------------------
% create_process() - process creation function
% This function create individual process for
% each node.
%-----------------------------------------------------

create_process(WEs,MPid,MsgQueue) -> [{nth(1,Vs),spawn(mst,process,[MPid,Vs,MsgQueue])}||Vs<-WEs].

%-----------------------------------------------------
%
%    process(MPid,Vs) : This function calls process/7
%    ******* Vs = WEs *******
%-----------------------------------------------------

process(MPid,Vs,MsgQueue) ->
      X_=queue:new(),
      process(MPid,Vs,[],[dummy,dummy,sleeping,dumm,[{dumm,dumm,dumm,dumm}]],[],nil,X_).
      



%------------------------------------------------------
% process() - this is the code runs in every process()
% This process() which is process/4 that remains alive
% in everz process. In other words each node remains
% active for processin till end.
%------------------------------------------------------

process(MPid,Vs,PLs,SEs,DEs,TestEdge,MsgQueue)->

         F_=[1||{U,V,W,X}<-nth(5,SEs),(X==basic)],
         if
           F_==[] ->
                       F11_=[lists:sort([U,V])||{U,V,W,X}<-nth(5,SEs),(X==branch)],
                       F1_=F11_,
                       if
                         F1_==[] ->
                                  true;
                       F1_/=[] -> MPid!{complete,nth(1,Vs),F1_}
                       end;
          true->  true
         end,

     receive


	     {processlist,PLs1}->
               		process(MPid,Vs,PLs1,SEs,[],nil,[]);


             wakeup -> 
               		SEs_new=wakeup(Vs,PLs),               %% SEs_new = [LN,MW,SN,FC,[{node,dest node, weight, basic/branch/rejected},{...}...] ]
                        process(MPid,Vs,PLs,SEs_new,DEs,TestEdge,MsgQueue);

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
                                      file:write_file("ms_connect_sleep.txt", io_lib:format("~p~n", [Z]), [append]),
 				                                         

				      if 
			 		 L < B -> 
                                                     SEs_new = edge_update(nth(5,Z),Node),
                                                     SPid!{initiateMessage,nth(1,Z),nth(2,Z),nth(3,Z),nth(1,Vs)},
                                                     S=nth(3,Z),
       						     if 
                                               		  S==find -> 
                                                               FC=nth(4,Z)+1,
                                                               ZZ=[nth(1,Z),nth(2,Z),nth(3,Z),FC|[SEs_new]],
                                                               process(MPid,Vs,PLs,ZZ,DEs,TestEdge,MsgQueue);
                                               		  S/=find ->
                                                               ZZZ=[nth(1,Z),nth(2,Z),nth(3,Z),nth(4,Z)|[SEs_new]],
                                                               process(MPid,Vs,PLs,ZZZ,DEs,TestEdge,MsgQueue)
                                                     end;
					L>=B -> 
                                                 L1 = nth(5,Z),
                                                 R=[1||{U,V,W,X}<-L1,(V==Node) and (X==basic)],   % Here R is for checking if the edge is branch or basic
                                                 if
                                                    R==[1] -> 
                                                               MsgQ_=queue:in({connect,L,Node,SPid,MW},MsgQueue),  % ToDo how to put the request on queue  
                                                               process(MPid,Vs,PLs,Z,DEs,TestEdge,MsgQ_); 
                                                    R==[] -> SPid!{initiateMessage,nth(1,Z)+1,MW,find,nth(1,Vs)},
                                                             SEs_new_1 = [nth(1,Z)+1,MW,find,nth(1,Z)|[nth(5,Z)]],
                                                             process(MPid,Vs,PLs,SEs_new_1,DEs,TestEdge,MsgQueue);   
                                                    true -> true
					         end
				      end; 
%%-----------------------------------------------------------------------------------------
%%
%%     This below portion of code will be called when the node is NOT in sleeping mode.
%%
%%-----------------------------------------------------------------------------------------

                          (A /= sleeping) -> 
                                      Z=SEs,                                      % Since the node is sleeping, wakeup process will be called.
                                      B=nth(1,Z),                                 % Here variable B stores the Level Number (LN) returned by wakeup process. 		                                         

				      if 
			 		 L < B -> 
                                                     SEs_new = edge_update(nth(5,Z),Node),
                                                     SPid!{initiateMessage,nth(1,Z),nth(2,Z),nth(3,Z),nth(1,Vs)},
                                                     S=nth(3,Z),
       						     if 
                                               		  S==find -> 
                                                          	FC=nth(4,Z)+1,
                                                          	ZZ=[nth(1,Z),nth(2,Z),nth(3,Z),FC|[SEs_new]],
                                                          	process(MPid,Vs,PLs,ZZ,DEs,TestEdge,MsgQueue);
                                               		  S/=find ->
                                                                ZZZ=[nth(1,Z),nth(2,Z),nth(3,Z),nth(4,Z)|[SEs_new]],
                                                               	process(MPid,Vs,PLs,ZZZ,DEs,TestEdge,MsgQueue)
                                                     end;
                          		L>=B -> 
                                                 L1 = nth(5,Z),
                                                 R=[1||{U,V,W,X}<-L1,(V==Node) and (X==basic)],   % Here R is for checking if the edge is branch or basic
                                                 %file:write_file("ms_con_q.txt", io_lib:format("~p~n~p~n", [SEs,R]), [append]),

                                                 if
                                                    R==[1] -> 
                                                               MsgQ1=insert_queue(MsgQueue,{connect,L,Node,SPid,MW}),  % ToDo MsgQueue
                                                               process(MPid,Vs,PLs,SEs,DEs,TestEdge,MsgQ1); 
                                                    R==[] -> 
							     SPid!{initiateMessage,nth(1,Z)+1,MW,find,nth(1,Vs)},
                                                             SEs_new_1 = [nth(1,Z)+1,MW,find,nth(1,Z)|[nth(5,Z)]],

                                                             process(MPid,Vs,PLs,SEs_new_1,DEs,TestEdge,MsgQueue);
                                                    true -> true
                                                
					         end
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
                                           SEs_new=[L,F,S,FC|[nth(5,SEs)]],
                                           TestEdge1=testMessage(Vs,SEs_new,DEs_new,nth(5,SEs_new),PLs,nth(1,SEs_new),nth(2,SEs_new)),
                                           process(MPid,Vs,PLs,SEs_new,DEs_new,TestEdge1,MsgQueue);    % MPid,Vs,PLs,SEs,[in-branch,best-edge,best-weight],test-weight
                                               
                                 S/=find ->
                                           SEs_new=[L,F,S,nth(4,SEs)|[nth(5,SEs)]],
                                           TestEdge2=nil,
                                           process(MPid,Vs,PLs,SEs_new,DEs_new,TestEdge2,MsgQueue)           % MPid,Vs,PLs,SEs,[in-branch,best-edge,best-weight],test-weight                                         

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
          if
              (L>=LN) ->
                       if
                          (F/=FN) -> 
                                     [Pid!{accept,nth(1,Vs)}||{N,Pid}<-PLs, (N==Node)],    % N is node for Pid, sending accept message on edge j
                                     process(MPid,Vs,PLs,SEs_new,DEs,TestEdge,MsgQueue);
                          (F==FN) ->  
                                     XX=nth(5,SEs_new),
                                     [{Label,Weight}]=[{X,W}||{U,V,W,X}<-XX,(V==Node),(X==basic)],
                                    if 
                                       (Label==basic) ->
						     L1=[{U,V,W,rejected}||{U,V,W,X}<-XX,(V==Node),(X==basic)],
                                                     L2=[{U,V,W,X}||{U,V,W,X}<-XX,(V/=Node)],
                                                     SEs_1=L1++L2,
                                                     Z_=([nth(1,SEs_new),nth(2,SEs_new),nth(3,SEs_new),nth(4,SEs_new)]++[SEs_1]),
                                                     if 
                                                        (Weight /= TestEdge) -> 
                                                          
                                                          [Pid!{reject,nth(1,Vs)}||{N,Pid}<-PLs, (N==Node)],
                                                          process(MPid,Vs,PLs,Z_,DEs,TestEdge,MsgQueue);
                                                         
                                                        (Weight == TestEdge) ->
                                                          TestEdge1=testMessage(Vs,Z_,DEs,SEs_1,PLs,nth(1,SEs_new),nth(1,SEs_new)),
                                                          process(MPid,Vs,PLs,Z_,DEs,TestEdge1,MsgQueue)
                       
                                                     end;
                                                                                                                                                            
                                       (Label /= basic) ->
                                              process(MPid,Vs,PLs,SEs_new,DEs,TestEdge,MsgQueue)
                       
                                                     
                                    end  
                        end;
              
              (L<LN)-> 
                        MsgQ1=insert_queue(MsgQueue,{test,LN,FN,Node}),     % ToDo -- L>LN, create the message queue                            
                        process(MPid,Vs,PLs,SEs_new,DEs,TestEdge,MsgQ1)

          end;

       (V_sleep/=sleeping)->
            L=nth(1,SEs),
            F=nth(2,SEs),
          if
              (L>=LN) ->
                       if
                          (F/=FN) -> 
                                     [Pid!{accept,nth(1,Vs)}||{N,Pid}<-PLs, (N==Node)],    % N is node for Pid, sending accept message on edge j
                                     process(MPid,Vs,PLs,SEs,DEs,TestEdge,MsgQueue);
                          (F==FN) ->  
                                     XX=nth(5,SEs),
                                     LabelWeight=[{X,W}||{U,V,W,X}<-XX,(V==Node),(X==basic)],
                                    if
                                       length(LabelWeight)==0 -> Label=abc, Weight=0;
                                       length(LabelWeight)/=0 -> Label=basic, Weight=element(2,nth(1,LabelWeight))
                                    end,
                                    if 
                                       (Label==basic) ->
						     L1=[{U,V,W,rejected}||{U,V,W,X}<-XX,(V==Node),(X==basic)],
                                                     L2=[{U,V,W,X}||{U,V,W,X}<-XX,(V/=Node)],
                                                     SEs_1=L1++L2,
                                                     Z_=([nth(1,SEs),nth(2,SEs),nth(3,SEs),nth(4,SEs)]++[SEs_1]),
                                                     if 
                                                        (Weight /= TestEdge) -> 
                                                          
                                                          [Pid!{reject,nth(1,Vs)}||{N,Pid}<-PLs, (N==Node)],
                                                          process(MPid,Vs,PLs,Z_,DEs,TestEdge,MsgQueue);
                                                         
                                                        (Weight == TestEdge) ->
                                                          TestEdge1=testMessage(Vs,Z_,DEs,SEs_1,PLs,L,F),
                                                          process(MPid,Vs,PLs,Z_,DEs,TestEdge1,MsgQueue)
                       
                                                     end;
                                                                                                                                                            
                                       (Label /= basic) ->
                                                      process(MPid,Vs,PLs,SEs,DEs,TestEdge,MsgQueue)
                       
                                    end  
                        end;
              
              (L<LN) ->       
                        MsgQ1=insert_queue(MsgQueue,{test,LN,FN,Node}), % ToDo -- L>LN, create the message queue 
                        process(MPid,Vs,PLs,SEs,DEs,TestEdge,MsgQ1)
          end

       end;

{accept,Node} ->
       TestEdge1=nil,
       [Wj]=[W||{U,V,W,X}<-nth(5,SEs),(V==Node)],
       BestWeight=nth(3,DEs),
       if
           (Wj<BestWeight) ->
                             BEdge=Node,
                             BWeight=Wj,
                             DEs_new=[Node,BEdge,BWeight],
                             SN_ = report(Vs,PLs,SEs,DEs_new,TestEdge),
                             process(MPid,Vs,PLs,([nth(1,SEs),nth(2,SEs),SN_,nth(4,SEs),nth(5,SEs)]),DEs_new,TestEdge1,MsgQueue);
           (Wj>=BestWeight) ->
                            SN_ = report(Vs,PLs,SEs,DEs,TestEdge),
                            process(MPid,Vs,PLs,([nth(1,SEs),nth(2,SEs),SN_,nth(4,SEs),nth(5,SEs)]),DEs,TestEdge1,MsgQueue)

       end;

{reject,Node} ->
          [Sj]=[X||{U,V,W,X}<-nth(5,SEs),(V==Node)],
          if 
            (Sj==basic) -> 
                  L1=[{U,V,W,rejected}||{U,V,W,X}<-nth(5,SEs),(V==Node)],
                  L2=[{U,V,W,X}||{U,V,W,X}<-nth(5,SEs),(V/=Node)],
                  SEs_1=L1++L2,
                  ZZ_=([nth(1,SEs),nth(2,SEs),nth(3,SEs),nth(4,SEs)]++[SEs_1]),
                  TestEdge1=testMessage(Vs,ZZ_,DEs,SEs_1,PLs,nth(1,SEs),nth(2,SEs)),
                  process(MPid,Vs,PLs,ZZ_,DEs,TestEdge1,MsgQueue);
            (Sj/=basic) -> 
                 process(MPid,Vs,PLs,SEs,DEs,TestEdge,MsgQueue)

          end;

{report,Node,BW}->
                 InBranch=nth(1,DEs),
                 BWeight=nth(3,DEs),
                 if 
                   (InBranch /= Node) ->
                                         FC_ = nth(4,SEs) -1,
                                         if
                                            (BW<BWeight) ->
                                                            BestWeight=BW,
                                                            BestEdge=Node,
                                                            DEs_=[InBranch,BestEdge,BestWeight],
                                             SN_ = report(Vs,PLs,SEs,DEs_,TestEdge),
                                             process(MPid,Vs,PLs,([nth(1,SEs),nth(2,SEs),SN_,FC_,nth(5,SEs)]),DEs_,TestEdge,MsgQueue);
                                            (BW>=BWeight) ->
                                             process(MPid,Vs,PLs,SEs,DEs,TestEdge,MsgQueue)
                                         end;
                   
                   (InBranch == Node) ->
                                        SN_1=nth(3,SEs),
                                        if
                                          (SN_1==find) ->    
							MsgQ1=insert_queue(MsgQueue,{report,Node,BW}),       % ToDo -- L>LN, create the message queue
                                             		process(MPid,Vs,PLs,SEs,DEs,TestEdge,MsgQ1);
                                          (SN_1/=find) ->
                                                          if 
                                                             (BW>BWeight) ->
                                                                    BEdge=changeCore(Vs,PLs,SEs,DEs),
                                                                    process(MPid,Vs,PLs,SEs,DEs,TestEdge,MsgQueue);   % ToDo : change for BEdge
                                                             (BW=<BWeight) ->
                                                                     if 
                                                                         ((BW==BWeight) and (BWeight==infinit)) ->
                                                                                  MPid!{halt,SEs};
                                                                          true ->
                                                                               process(MPid,Vs,PLs,SEs,DEs,TestEdge,MsgQueue)
                                                                     end
                                                          end 
                                        end

                end;

{changecore,Vs,PLs,SEs,DEs} ->
         BEdge=changeCore(Vs,PLs,SEs,DEs),
         process(MPid,Vs,PLs,SEs,DEs,TestEdge,MsgQueue)                                          
                                          
end.


%%-------------------------------------------------------------------------------------
%%
%%  Here below all the helper functions that are called from above portion of code
%%
%%-------------------------------------------------------------------------------------

%----------------------
% testMessage() 
%----------------------

testMessage(Vs,SEs,DEs,T,PLs,LN,FN)->

              Tmp_lst=[W||{U,V,W,X}<-T,(X==basic)],   % Create temporarz list with adj edge basic
              if 
                length(Tmp_lst)>0 ->
                		MW=min([W||{U,V,W,X}<-T,(X==basic)]),                 % Here find minimum weight of adjacent node with label basic
                		[{Src_Node,Node}]=[{U,V}||{U,V,W,X}<-T,(W==MW)],      % Destination with min weight edge
                		[B!{test,LN,FN,Src_Node}||{A,B}<-PLs,(A==Node)],
                                MW;                                                   % MW will be returned to be assigned to test-edge
                length(Tmp_lst)=<0 ->
                                report(Vs,PLs,SEs,DEs,nil),           % ToDo -- here start report.
                                nil                                   % nil will be returned to be assigned to test-edge


              end.
              


                 

%--------------------------------------
%   report(PLs,SEs,DEs,TestEdge)
%--------------------------------------

report(Vs,PLs,SEs,DEs,TestEdge) ->
     FC=nth(4,SEs),
     BWeight=nth(3,DEs),
     [InBranchPid]=[Pid||{Node,Pid}<-PLs,(Node==nth(1,DEs))],
     if 
	((TestEdge==nil) and (FC==0)) ->
               InBranchPid!{report,nth(1,Vs),nth(3,DEs)},
               SN=found;
     true->
               SN=nth(3,SEs)
     
     
     end,
    SN.


%----------------------
% changeCore()
%----------------------

  
changeCore(Vs,PLs,SEs,DEs)->
   Node_=nth(2,DEs),
   BEdge=[X||{U,V,W,X}<-nth(SEs,5),(V==Node_)],
   LN=nth(1,SEs),
   Name=nth(1,Vs),
   [Pid]=[Pid||{Node,Pid}<-PLs,(Node==Node_)],
   if (BEdge==branch) ->
                Pid!{changecore,Vs,PLs,SEs,DEs},
                ok;
   (BEdge/=branch) ->
                Pid!{connect,LN,Name,self(),nth(3,DEs)},
                branch
    end.  
              

     

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
            SEs_2=[{U,V,W,X}||{U,V,W,X}<-Ls, (V/=Node)],
            [SEs_1|SEs_2].
insert_queue(Arg1,Arg2) ->
                MQ_=queue:from_list(Arg1),
		MsgQ1_=queue:in(Arg2,MQ_),  % ToDo how to put the request on queue
                MList=queue:to_list(MsgQ1_),
                MList.
