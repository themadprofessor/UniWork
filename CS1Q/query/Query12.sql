SELECT member.forname, member.surname
FROM 2258082r.StaffMember AS member, 2258082r.StaffMember AS supervisor
WHERE member.staffNo=supervisor.staffNo;