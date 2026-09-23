//W910J028 JOB (650W9100500W910J028,W100),'RTN W910P1',                         
//             USER=?,PASSWORD=?,                                               
//         CLASS=K                                                              
/*JOBPARM FORMS=1800                                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W910    EXEC W910P028                                                         
//SOP     EXEC WSOPEND,PROCESS=W910J028                                         
/*                                                                              
