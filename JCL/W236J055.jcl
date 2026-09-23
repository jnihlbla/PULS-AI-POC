//W236J055 JOB (640W2360100W236J055,W100),'RTN W236R1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W236    EXEC W236P055                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W236J055                                         
