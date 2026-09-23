//W271J309 JOB (640W2710100W271J309,W100),'RTN W271DB',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//W271    EXEC W271P009,REFILL=DB,                                              
//             INDUT2=W271.W271DB                                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J309                                         
