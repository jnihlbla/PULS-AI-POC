//W271J340 JOB (640W2710100W271J340,W100),'RTN W271DA',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P040,                                                        
//             INDUT=W271.W271DA                                                
//*                                                                             
//SORT1.SYSIN     DD DSN=W.PROD.CONSTANT(W271PD4C),DISP=SHR                     
//*                                                                             
//SORT2.SYSIN     DD DSN=W.PROD.CONSTANT(W271PD4C),DISP=SHR                     
//*                                                                             
//W27140.STEERFIL DD DSN=W.PROD.CONSTANT(W271PD4F),DISP=SHR                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J340                                         
