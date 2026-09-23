//W115J027 JOB (650W1150100W115J027,W100),'RTN W115D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
/*AFTER MEMOAPIX                                                                
//*---  WMEMOSND,EXC                                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W115    EXEC W115P027                                                         
//*                                                                             
//TOMTST  EXEC WEMPTST,DSIN=W115.W115D1.W11527(+1)                              
//*                                                                             
//MEMOT   EXEC WMEMOSND,CONDS='(0,LT,TOMTST.T)'                                 
)SEND                                                                           
  TITLE DISPL W11527                                                            
  OPTION FORCE                                                                  
  DEST WSYST@VOLVOCARS.COM                       IT23.WHELP                     
  MEMO MEMTXTD1                                                                 
)END                                                                            
//MEMTXTD1 DD DSN=W.QASE.CONSTANT(W115M27H),DISP=SHR                            
//         DD DSN=W115.W115D1.W11527(+1),DISP=SHR                               
//*                                                                             
//SYSABEND DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W115J027                                         
