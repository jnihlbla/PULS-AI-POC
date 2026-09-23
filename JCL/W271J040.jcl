//W271J040 JOB (640W2710100W271J040,W100),'RTN W271D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P040,                                                        
//             INDUT=W271.W271D1                                                
//*                                                                             
//SORT1.SYSIN     DD DSN=W.PROD.CONSTANT(W271PD4A),DISP=SHR                     
//*                                                                             
//SORT2.SYSIN     DD DSN=W.PROD.CONSTANT(W271PD4B),DISP=SHR                     
//*                                                                             
//W27140.STEERFIL DD DSN=W.PROD.CONSTANT(W271PD4D),DISP=SHR                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J040                                         
