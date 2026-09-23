//W011J474 JOB (640W0110100W011J474,W100),'RTN W011S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W011    EXEC W011P074,                                                        
//             INDUT=W011.W011S1                                                
//*                                                                             
//W01174.W01174D4 DD DUMMY                                                      
//W01174.W01174D5 DD DUMMY                                                      
//W01174.W01174D6 DD DUMMY                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011J474                                         
