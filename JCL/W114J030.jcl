//W114J030   JOB (670W1140100W114J030,W100),'RTN W114D2',                       
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*ROUTE XEQ LOCAL                                                               
//W114P030 EXEC W114P030                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W114J030                                            
