//W611D7RS JOB (640W6110100W611D7RS,W100),'RTN W611D7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W611D7                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1=W611.W611V4.W6118H,                                             
//           T1=W611.W611D7.W6118H,RF1=FB,LR1=07                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611D7RS                                         
