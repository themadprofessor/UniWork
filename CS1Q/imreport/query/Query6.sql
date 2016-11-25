SELECT StaffMember.forename, StaffMember.surname
FROM 2258082r.StaffMember
JOIN 2258082r.LecturerCourse ON StaffMember.staffNo=LecturerCourse.lecturerId
JOIN 2258082r.Course ON LecturerCourse.courseId=Course.courseId
WHERE Course.title='CS-1Q' AND StaffMember.status='professor';