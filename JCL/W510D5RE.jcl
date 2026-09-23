//W510D5RE JOB (650W5100100W510D5RE,W100),'RTN W510D5',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//FREE    EXEC WFREE,NAME=W510D5,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W510D5RE                                         
