//W414D2RS JOB (640W4140100W414D2RS,W100),'RTN W414D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE  PRINT LOCAL                                                            
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W414D2                                               
//*                                                                             
//RENAME1 EXEC WRTNINP,                                                         
//           F1='W414.W414D1.W414X6',                                           
//           T1='W414.W414D2.W414X6',RF1=VB,LR1=73                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W414D2RS                                         
