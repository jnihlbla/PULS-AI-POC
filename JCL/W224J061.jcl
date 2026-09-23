//W224J061 JOB (640W2240100W224J061,W100),'RTN W224V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W224    EXEC W224P061                                                         
//*                                                                             
//W22461.W22461D1 DD *                                                          
7179                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W224J061                                         
