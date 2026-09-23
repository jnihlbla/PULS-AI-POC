//WF10D4RE JOB (640WF100100WF10D4RE,W100),'RTN WF10D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTF                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//FREE    EXEC WFREE,NAME=WF10D4,MAXRC=8                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10D4RE                                         
