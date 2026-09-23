//W910J050 JOB (540W9100100W910J050,W100),'RTN W91050',                         
//         CLASS=K,USER=?,PASSWORD=?                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//         EXEC W910P050                                                        
//SOP     EXEC WSOPEND,PROCESS=W910J050                                         
/*                                                                              
