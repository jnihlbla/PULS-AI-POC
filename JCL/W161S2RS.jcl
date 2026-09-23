//W161S2RS JOB (640W1610100W161S2RS,W100),'RTN W161S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W161S2                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1=W161.W161X2SE.W16152,                                           
//           T1=W161.W161S2.W16152,RF1=FB,LR1=0256                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W161S2RS                                         
