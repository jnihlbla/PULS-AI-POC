//W219J101 JOB (640W2190100W219J101,W100),'RTN W440V3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W219    EXEC W219P101                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W219J101                                         
