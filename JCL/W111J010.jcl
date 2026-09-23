//W111J010 JOB (640W1110100W111J010,W100),'RTN W111V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W111    EXEC W111P010                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W111J010                                         
