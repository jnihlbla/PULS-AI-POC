//W214J022 JOB (640W2140100W214J022,W100),'RTN W214D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0,LINES=500                                         
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W214    EXEC W214P022                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W214J022                                         
