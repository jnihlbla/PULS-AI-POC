//W271J051 JOB (640W2710100W271J051,W100),'RTN W271R3',                         
//             CLASS=K,USER=?,PASSWORD=?,LINES=999                              
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P051                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J051                                         
