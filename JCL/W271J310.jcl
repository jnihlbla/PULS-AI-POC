//W271J310 JOB (640W2710100W271J310,W100),'RTN W271DB',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P010,REFILL=DB,                                              
//             INDUT=W271.W271DB                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J310                                         
