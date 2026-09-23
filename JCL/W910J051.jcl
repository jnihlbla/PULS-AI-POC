//W910J051 JOB (650W9100500W910J051,W100),'RTN W91054',                         
//             USER=?,PASSWORD=?,                                               
//         CLASS=K                                                              
/*JOBPARM FORMS=1800                                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W910     EXEC W910P051                                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W910J051                                         
