//W271J077 JOB (640W2710100W271J077,W100),'RTN W271V3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P077                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J077                                         
