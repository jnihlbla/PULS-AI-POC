//W011J784 JOB (640W0110100W011J784,W100),'RTN W512B1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W011    EXEC W011P084                                                         
//*                                                                             
//SORT04.SORTOUT  DD DUMMY                                                      
//SORT05.SORTOUT  DD DUMMY                                                      
//SORT06.SORTOUT  DD DUMMY                                                      
//SORT07.SORTOUT  DD DUMMY                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011J784                                         
