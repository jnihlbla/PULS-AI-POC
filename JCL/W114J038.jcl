//W114J038   JOB (650W1140100W114J038,W100),'RTN W114D1',                       
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800                                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*ROUTE XEQ LOCAL                                                               
//W114P038 EXEC W114P038                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W114J038                                            
