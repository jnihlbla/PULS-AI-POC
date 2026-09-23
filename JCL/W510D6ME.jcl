//W510D6ME JOB (640W5100100W510D6ME,W100),'RTN W510D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*                      /CNTL  WMEMOSND,EXC                                    
//*                                                                             
//TOMTEST EXEC WEMPTST,DSIN=W510.W510D6.W51014(0)                               
//*                                                                             
//WMEMOSND EXEC WMEMOSND,CONDS='(4,GT,TOMTEST.T)',                              
//             REQS=W510D6ME                                                    
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W510D6ME                                            
