SELECT
  s.student_Number AS StudentNumber
, s.SCHED_YEAROFGRADUATION AS schedGradYear
,  DECODE(s.enroll_status
         , 3, 1
         , 0)AS graduated
, scf.GRADUATION_YEAR AS gradYear
, reGrad.schoolID AS gradSchool
, sg.grade_level AS gradeLevel
, sg.course_name AS courseName 
, sg.grade AS score 
, sg.PERCENT AS percentScore 
, sg.CREDIT_TYPE
, TO_CHAR(sg.DATESTORED,'mm/dd/yyyy') AS dateStored
FROM Students s
INNER JOIN StudentCoreFields scf 
ON s.DCID = scf.STUDENTSDCID
INNER JOIN ReEnrollments reGrad
ON s.ID = reGrad.studentID
INNER JOIN StoredGrades sg 
ON s.ID = sg.studentID 
INNER JOIN Courses c 
ON sg.course_number = c.course_number 
WHERE reGrad.GRADE_LEVEL = 12
AND sg.GRADE_LEVEL BETWEEN 7 AND 12
AND sg.STORECODE IN ('S1','S2');
