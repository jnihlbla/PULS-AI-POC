//W512M4RS JOB (640W5120100W512M4RS,W100),'RTN W512M4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W512M4                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1=WF10.WF10M2.WF1091,                                             
//           T1=WF10.W512M4.WF1091,RF1=FB,LR1=135                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W512M4RS                                         
