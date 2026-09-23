//W551B9RS JOB (640W5510100W551B9RS,W100),'RTN W551B9',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W551B9                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1='W553.W553B4.W55307',                                           
//           T1='W553.W551B9.W55307',RF1=FB,LR1=250                             
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551B9RS                                         
