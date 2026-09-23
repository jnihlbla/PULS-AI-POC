//W271J007 JOB (640W2710100W271J007,W100),'RTN W271V4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W271    EXEC W271P007                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J007                                         
