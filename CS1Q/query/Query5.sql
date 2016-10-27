SELECT StudentCourse.examMark, Student.forename, Student.surname
FROM 2258082r.StudentCourse
JOIN 2258082r.Course ON StudentCourse.courseId=Course.courseId
JOIN 2258082r.Student ON StudentCourse.studentId=Student.matricNo
WHERE Course.title='CS-1Q'
ORDER BY Student.surname ASC;