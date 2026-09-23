//W500V1RE JOB (650W5100100W500V1RE,W100),'RTN W500V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//FREE    EXEC WFREE,NAME=W500V1,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W500V1RE                                         
