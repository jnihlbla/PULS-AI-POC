//W111B1RS JOB (640W1110100W111B1RS,W100),'RTN W111B1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W111B1                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1=W111.W111X1SE.W11137,                                           
//           T1=W111.W111B1.W11137,RF1=VB,LR1=255                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W111B1RS                                         
