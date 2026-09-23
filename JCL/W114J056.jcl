//W114J056 JOB (670W1140100W114J056,W100),'RTN W114D5',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM ROOM=ARHK,FORMS=1800,LINECT=0                                         
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//PROC JCLLIB ORDER=(W.QASE.PROCLIB)                                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
//     INCLUDE MEMBER=SYST2                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
//*                                                                             
//W114P056 EXEC W114P056                                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114J056                                         
