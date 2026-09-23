//W114J040   JOB (670W1140100W114J040,W100),'RTN W114D1',                       
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
/*JOBPARM FORMS=1800                                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*ROUTE XEQ LOCAL                                                               
//W114P040 EXEC W114P040                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W114J040                                            
