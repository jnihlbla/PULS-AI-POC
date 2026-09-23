//W512M4RE JOB (640W5120100W512M4RE,W100),'RTN W512M4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//FREE    EXEC WFREE,NAME=W512M4,MAXRC=8                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W512M4RE                                         
