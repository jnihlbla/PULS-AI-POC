//W225J040 JOB (640W2250100W225J040,W100),'RTN W225V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W225    EXEC W225P040                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W225J040                                         
