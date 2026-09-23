//W224J261 JOB (640W2240100W224J261,W100),'RTN W224V4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W224    EXEC W224P261                                                         
//*                                                                             
//W22461.W22461D1 DD *                                                          
4149                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W224J261                                         
