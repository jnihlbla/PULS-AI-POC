//W161S3RS JOB (640W1610100W161S3RS,W100),'RTN W161S3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W161S3                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1=W161.W161X3SE.W16150,                                           
//           T1=W161.W161S3.W16150,RF1=FB,LR1=0256                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W161S3RS                                         
