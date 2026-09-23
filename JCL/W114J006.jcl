//W114J006   JOB (670W1140100W114J006,W100),'RTN W114V1',                       
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*ROUTE XEQ LOCAL                                                               
//W114P006 EXEC W114P006                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W114J006                                            
