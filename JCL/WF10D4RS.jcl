//WF10D4RS JOB (640WF100100WF10D4RS,W100),'RTN WF10D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTF                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=WF10D4                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1=WF10.WF10X6SE.WF1050,                                           
//           T1=WF10.WF10D4.WF1050,RF1=VB,LR1=1058,CP1=5                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10D4RS                                         
