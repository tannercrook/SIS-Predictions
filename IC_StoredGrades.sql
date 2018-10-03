SELECT 
  p.studentNumber AS studentNumber
, ISNULL(g.onTimeGradYr, YEAR(g.diplomaDate)) AS schedGradYear 
, CASE 
	WHEN g.diplomaDate IS NOT NULL THEN 1
	ELSE 0
  END AS graduated 
, YEAR(g.diplomaDate) AS gradYear
, gs.number AS gradSchool 
, CAST(tc.grade AS INT) AS gradeLevel
, tc.courseName AS courseName
, tc.score AS score
, tc.[percent] AS percentScore
, cs.name AS creditType
, CONVERT(VARCHAR(10), tc.date, 101) AS dateStored
FROM Person p 
INNER JOIN [Identity] i 
ON p.currentIdentityID = i.identityID
INNER JOIN TranscriptCourse tc 
ON p.personID = tc.personID
INNER JOIN TranscriptCredit tcc 
ON tc.transcriptID = tcc.transcriptID
INNER JOIN CurriculumStandard cs 
ON tcc.standardID = cs.standardID
LEFT JOIN Graduation g 
ON p.personID = g.personID
LEFT JOIN School gs 
ON g.schoolID = gs.schoolID
WHERE g.onTimeGradYr <= 2018
AND CAST(tc.grade AS Int) BETWEEN 7 AND 12;
