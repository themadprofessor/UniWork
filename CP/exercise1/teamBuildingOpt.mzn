%
% Given a set of students produce teams
% Each student is one team and one team only
% A team must be of size lwb to upb inclusive
% There are some pairs of students who must be in the same team together
% There are some pairs of students who must not be in the same team together
%
% There are two parts to the exercise
%
%  (a) Produce T teams, i.e. produce a satisfactory allocation of students to teams
%  (b) Minimize the number of teams produced
%
include "globals.mzn";

enum STUDENTS;

int: T; % number of teams
int: LWB; % minimum number of students in a team
int: UPB; % maximum number of students in a team
set of int: TEAM = 1..T;
array[int,1..2] of STUDENTS: together; % pairs of students who must be together
array[int,1..2] of STUDENTS: apart; % pairs of students who must be apart

set of int: pairsApart = index_set_1of2(apart); % range of indices 
set of int: pairsTogether = index_set_1of2(together); % as above

% NOTE: the number of students in team t can be zero or in the range LWB..UPB

set of int: teamSize = LWB..UPB union {0};

array[0..T,STUDENTS] of var {0,1}: teams;

% A student can only be in one team
constraint forall(student in STUDENTS)(sum(team in 0..T)(teams[team,student]) = 1);

% A team must either be empty or within the bounds of team sizes
constraint forall(team in 0..T)(sum(student in STUDENTS)(teams[team,student]) in teamSize);

% Students which must be apart are apart
constraint forall(index in pairsApart)(forall(team in 0..T)((teams[team,apart[index,1]] + teams[team,apart[index,2]]) < 2));

% Students which must be together are togther
constraint forall(index in pairsTogether)(forall(team in 0..T)((teams[team,together[index,1]] + teams[team,together[index,2]]) in {0,2}));

% All teams are equal
constraint forall(team1,team2 in 0..T where team1 < team2)(lex_greatereq(row(teams,team1),row(teams,team2)));

% Number of teams with students in
var int: non_empty_teams = sum(team in 0..T)(sum(student in STUDENTS)(teams[team, student]) != 0);

solve minimize non_empty_teams;

output["pairs apart: = \(pairsApart) \n"];
output["pairs together: = \(pairsTogether) \n"];
output["teamSize: = \(teamSize) \n"];
output["nonEmptyTeams: = \(non_empty_teams) \n"];
output["teams: \n"];
output[show(student) ++ if student=max(STUDENTS) then "\n" else "," endif | student in STUDENTS];
output["" ++ format(string_length(show(student)), 10, teams[team,student]) ++ if student=max(STUDENTS) then "\n" else "," endif | team in 0..T, student in STUDENTS];
