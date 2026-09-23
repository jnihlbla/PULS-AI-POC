//W219J007 JOB (640W2190100W219J007,W100),'RTN W219V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W219    EXEC W219P007                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W219J007                                         
