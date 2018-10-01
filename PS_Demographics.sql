SELECT
  s.student_Number AS StudentNumber
, s.SCHED_YEAROFGRADUATION AS schedGradYear
,  DECODE(s.enroll_status
         , 3, 1
         , 0)AS graduated
, scf.GRADUATION_YEAR AS gradYear
, reGrad.schoolID AS gradSchool
, NVL(ps_customfields.getStudentscf(s.id,'WY_LunchStatus'),'N') AS FreeReduced
, scf.primaryLanguage AS homeLanguage
, CASE ps_customfields.getStudentscf(s.id,'WY_IDEA') 
          WHEN 'Y' THEN 'Y'
          ELSE 'N' END AS IEP
FROM Students s
INNER JOIN StudentCoreFields scf 
ON s.DCID = scf.STUDENTSDCID
INNER JOIN ReEnrollments reGrad
ON s.ID = reGrad.studentID
WHERE reGrad.GRADE_LEVEL = 12;
