//W011J560 JOB (640W0110100W011J560,W100),'RTN W011D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W011    EXEC W011P060                                                         
//*                                                                             
//W01160.W01160D6 DD DUMMY                                                      
//W01160.W01160D7 DD DUMMY                                                      
//W01160.W01160D8 DD DUMMY                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011J560                                         
