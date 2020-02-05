
                                      The Team Building Problem
                                      -------------------------

Given a collection of students, build teams of students such that no team has less than LWB members,
no more than UPB members and each student is in exactly one team. In addition, we are given a list 
of pairs of students who must be apart, i.e. pairs of students who must not be in the same team 
together. Also, we have a list of pairs of students who must be in the same team together.

You must find an allocation of students to teams that satisfies these constraints.

Additionally, you might find an allocation to the minimum number of teams, such that this
minimises the supervision overhead.


Problem Instances
-----------------
There is a directory of problem instances, called data. Also, we have three instance to try:
instSmall, instMedium and instLarge.

Running your program
--------------------
The program must run from the command line take the input problem as an argument, for example
 
  > mzn-gecode teamBuilding.mzn instSmall.dzn -s

What you have to do
-------------------
1. Modify the program teamBuilding.mzn so that it finds a solution to instances in the data directory.
   To do this you will have to do the following

   - create appropriate constrained integer variables
   - add appropriate constraints to the model
   - do a search for satisfaction and also for optimization

2. Modify your model such that it produces an optimal solutio that uses the minimum number of teams.
   Call this file teamBuildingOpt.mzn

3. Write a short report (a .txt file will suffice, one page maximum) that should contain the following
     - description of your model
        - the variables and what they mean
        - the domains of those variables and what those mean
        - identification of the decision variables
        - the constraints, and again what these mean
        - any variable or value ordring heuristics used
     - mention/discuss any symmetries within your model and how they
       might be eliminated
     - typical performance
     - possible enhancements or alternative models that might be considered

4. email teamBuilding.mzn, teamBuildingOpt.mzn and report.txt as attachements to pat@dcs.gla.ac.uk with subject
   "CP(M) ex01" including in the body of the email your name, course of study and matriculation number


NOTE 1: this is 10% of the course mark and should take no more than 2 days of your time 
NOTE 2: you have to email me only 3 files as above
NOTE 3: your program must run on the command line as follows

        > mzn-gecode teamBuilding.mzn instSmall.dzn -s
        > mzn-gecode teamBuildingOpt.mzn instSmall.dzn -s

NOTE 4: failure to comply with the instructions will result in zero marks.



Patrick Prosser
