//W224J055 JOB (640W2240100W224J055,W100),'RTN W224V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W224    EXEC W224P055                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W224J055                                         
