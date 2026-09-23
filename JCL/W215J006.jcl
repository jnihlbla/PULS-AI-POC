//W215J006 JOB (670W2150100W215J006,W100),'RTN W215S2',                         
//             CLASS=1,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W215    EXEC W215P006                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W215J006                                         
