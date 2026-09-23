//W611J028 JOB (640W6110100W611J028,W100),'RTN W611S9',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W611    EXEC W611P028                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J028                                         
