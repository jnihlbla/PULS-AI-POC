//W910J021 JOB (650W9100500W910J021,W100),'RTN W910D2',                         
//             USER=?,PASSWORD=?,                                               
//         CLASS=K                                                              
/*JOBPARM FORMS=1800                                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W910     EXEC W910P021                                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W910J021                                         
