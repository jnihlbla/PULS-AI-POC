//W371JME1 JOB (640W3710100W371JME1,W100),'RTN W371D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*---  WMEMOSND,EXC                                                            
//*                                                                             
//CHECK   EXEC WEMPTST,DSIN=W371.W371D1.W371DUB(+0)                             
//EMPTEST IF  (CHECK.T.RC EQ 0) THEN                                            
//SEND    EXEC WMEMOSND                                                         
//APIFILE  DD  *                                                                
)SEND                                                                           
ETITLE ERROR IN RHM TRANSACTIONS FROM VIPS                                      
OPTION FORCE                                                                    
DEST SUPPORT.VIPS.CAR3RD@VOLVO.COM                                              
DEST SEPPO.SKOGBERG@VOLVO.COM                                                   
MEMO MAILDATA                                                                   
)END                                                                            
//MAILDATA DD  DSN=W371.W371D1.W371DUB(+0),DISP=SHR                             
//EMPTEST ENDIF                                                                 
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W371JME1                                            
