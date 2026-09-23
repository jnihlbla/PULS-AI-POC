//W414V1RE JOB (640W4140100W414V1RE,W100),'RTN W414V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//FREE    EXEC WFREE,NAME=W414V1,MAXRC=8                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W414V1RE                                         
