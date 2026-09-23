//W476DARE JOB (650W4760100W476DARE,W100),'RTN W476DA',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W476DA,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W476DARE                                         
