%
% Given a set of students produce teams
% Each student is one team and one team only
% A team must be of size lwb to upb inclusive
% There are some pairs of students who must be in the same team together
% There are some pairs of students who must not be in the same team together
% Minimize the number of teams produced
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

var LWB..UPB union {0}: teamSize;

solve satisfy;

output["pairs apart: = \(pairsApart) \n"];
output["pairs together: = \(pairsTogether) \n"];
output["teamSize: = \(teamSize) \n"];