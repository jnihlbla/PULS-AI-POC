//W510D4RE JOB (650W5100100W510D4RE,W100),'RTN W510D4',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//FREE    EXEC WFREE,NAME=W510D4,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W510D4RE                                         
