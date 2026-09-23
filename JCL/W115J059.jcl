//W115J059 JOB (640W1150100W115J059,W100),'RTN W115S4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W115    EXEC W115P059                                                         
//*                                                                             
//W01160.W01160D6 DD DUMMY                                                      
//W01160.W01160D7 DD DUMMY                                                      
//W01160.W01160D8 DD DUMMY                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W115J059                                         
