//W232J064 JOB (640W2320100W232J064,W100),'RTN W200V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W232    EXEC W232P064                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W232J064                                         
