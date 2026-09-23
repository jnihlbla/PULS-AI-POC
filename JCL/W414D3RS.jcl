//W414D3RS JOB (640W4140100W414D3RS,W100),'RTN W414D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W414D3                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1=W414.W414D1.W41412,                                             
//           T1=W414.W414D3.W41412,RF1=FB,LR1=0039                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W414D3RS                                         
