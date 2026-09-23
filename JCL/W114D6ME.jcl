//W114D6ME JOB (640W1140100W114D6ME,W100),'RTN W114D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*---  WMEMOSND,EXC                                                            
//*                                                                             
//EMPTY1  EXEC WEMPTST,DSIN=W114.W114D6.W11455(+0)                              
//*                                                                             
//    IF (EMPTY1.T.RC NE 4) THEN                                                
//*                                                                             
//MEMO    EXEC WMEMOSND,DSIN=NULLFILE                                           
//APIFILE  DD DSN=W114.W114D6.W11456(+0),DISP=SHR                               
//SEND     DD DSN=W.QASE.CONSTANT(W114D6ME),DISP=SHR                            
//         DD DSN=W114.W114D6.W11455(+0),DISP=SHR                               
//SYSABEND DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//*                                                                             
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114D6ME                                         
