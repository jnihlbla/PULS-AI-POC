//W233J120 JOB (640W2330100W233J120,W100),'RTN W233V3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W233    EXEC W233P020,INDUT=W233.W233V3                                       
//*                                                                             
//W23320.W23320D1 DD DUMMY                                                      
//W23320.W23320D2 DD DUMMY                                                      
//W23320.W23320D4 DD DUMMY                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W233J120                                         
