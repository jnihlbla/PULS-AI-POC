//W910J044 JOB (650W9100100W910J044,W100,1440,,,1800),'RTN W91044',             
//             USER=?,PASSWORD=?,                                               
//         CLASS=K                                                              
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//         EXEC W910P044                                                        
//SOP     EXEC WSOPEND,PROCESS=W910J044                                         
/*                                                                              
