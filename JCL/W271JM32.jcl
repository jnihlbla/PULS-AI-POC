//W271JM32 JOB (640W2710100W271JM32,W100),'RTN W271DA',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P032,REFILL=DC43,                                            
//             INDUT=W271.DC43                                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271JM32                                         
