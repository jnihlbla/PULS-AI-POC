//W271J140 JOB (640W2710100W271J140,W100),'RTN W271V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P040,                                                        
//             INDUT=W271.W271V1                                                
//*                                                                             
//SORT1.SYSIN     DD DSN=W.PROD.CONSTANT(WSRTARDC),DISP=SHR                     
//*                                                                             
//SORT2.SYSIN     DD DSN=W.PROD.CONSTANT(WSRTARDC),DISP=SHR                     
//*                                                                             
//W27140.STEERFIL DD DSN=W.PROD.CONSTANT(W271PD4E),DISP=SHR                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J140                                         
