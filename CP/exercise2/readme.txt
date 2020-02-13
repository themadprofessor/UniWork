Exercise 2: The Balanced Academic Curriculum Problem
----------------------------------------------------

The Balanced Academic Curriculum Problem (BACP) was introduced by Castro and Manzano in 2001
and may be defined as follows. We are given a set of n courses to be taught, where each course 
has a specified number of credits and prerequisites (courses that must be taken before this course) 
and these courses have to be taught in m periods subject to the following constraints:

  (0) Every course must be assigned to a period.
  (1) The number of courses taught in a period must be at least a 
      specified minimum and at most a specified maximum.
  (2) The number of credits taught in a period must be at least a 
      specified minimum and at most a specified maximum.
  (3) If a course c_j has course c_i as a prerequisite then course c_i must 
      be taught in some period before c_j.
  (4) The allocation of courses to periods must be balanced 

The problem instances come from the Department di Informatica, Federico Santa Maria Technical University at 
Valparasio. Three instances were made available: bacp08 has 8 periods, 46 courses and 37 prerequisite pairs; 
bacp10 has 10 periods, 42 courses and 34 prerequisite pairs; bacp12 has 12 periods, 66 courses and 65 prerequisite 
pairs. The BACP has been adopted by the CP community as problem prob030 in cspLib.


ToDo (part 1)
-------------
You are to edit the file BACP.java such that it models the balanced academic curriculum problem.
You must do as follows:

 (a) create the necessary constrained integer variables for your model
 (b) post the appropriate constraints
 (c) modify the optimize method
 (d) modify the solve method
 
 NOTE: you might consider using variable and value ordering heuristics in the solve and
       optimize methods.
 
 
 How To Run Your Model
 ---------------------
 To find a first solution to problem instance bacpTiny.txt in the data directory do as follows
 
  > java Go ../data/bacpTiny.txt -solve 
  
 The output can be captured and validated as follows
 
  > java Go ../data/bacpTiny.txt -solve  > sol.txt
  > java Validate ../data/bacpTiny.txt sol.txt
  
  To find an optimal solution to problem instance bacpTiny.txt in the data directory do as follows
  
  > java Go ../data/bacpTiny.txt -opt
  
  You might look at all the options available in the Go program (-solve, -opt, -time, -brief, -trace). 
  
  
  ToDo (part 2)
  -------------
  Attempt to find first solutions to all (11) problems in the data directory (i.e. use -solve option).
  Attempt to find optimal solutions to all (11) problems in the data directory (i.e. use -opt option).
  Take note of runtimes, number of nodes, quality of solution (imbalance and maximum credit load).
  
  ToDo (part 3)
  -------------
  Email me the file BACP.java and a short report (no more than 2 pages in length). The report should address
  the following:
  
    - Give a readable description of your model, stating the variables, their domains
      and what those variables actually mean.
    - Describe the constraints used, and what they actually mean.
    - For a problem with n courses and m periods, what will be the size of your model?
      That is, how does the model size grow with problem input?
    - Describe any variable and value ordering heuristics used.
    - Give a brief report on your computational experience solving and optimizing the problems.
    - Briefly describe at least one alternative model that might be considered.
	
	
	
Patrick Prosser
05/02/2020
	
  
  