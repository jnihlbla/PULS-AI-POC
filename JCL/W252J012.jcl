//W252J012 JOB (640W2520100W252J012,W100),'RTN W252D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W252    EXEC W252P012                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W252J012                                         
