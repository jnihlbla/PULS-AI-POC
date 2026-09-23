//W910J020 JOB (650W9100500W910J020,W100),'RTN W910D2',                         
//             USER=?,PASSWORD=?,                                               
//         CLASS=K                                                              
/*JOBPARM FORMS=1800                                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST0                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W910     EXEC W910P020                                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W910J020                                         
