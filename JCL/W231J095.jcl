//W231J095 JOB (640W2310100W231J095,W100),'RTN W231V3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W231    EXEC W231P095                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W231J095                                         
