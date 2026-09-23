//W225J042 JOB (640W2250100W225J042,W100),'RTN W225V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W225    EXEC W225P042                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W225J042                                         
