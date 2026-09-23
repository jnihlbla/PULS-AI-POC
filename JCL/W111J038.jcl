//W111J038 JOB (640W1110100W111J038,W100),'RTN W111B1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W111    EXEC W111P038                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W111J038                                         
