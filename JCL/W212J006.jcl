//W212J006 JOB (670W2120100W212J006,W100),'RTN W200D1',                         
//             CLASS=1,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W212    EXEC W212P006                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W212J006                                         
