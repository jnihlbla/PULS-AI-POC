//W560Y3RE JOB (650W5600100W560Y3RE,W100),'RTN W560Y3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//FREE    EXEC WFREE,NAME=W560Y3,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W560Y3RE                                         
