//WF10R3RS JOB (640WF100100WF10R3RS,W100),'RTN WF10R3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=WF10R3                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='WF10.WF10X1CN.WF1020',                                         
//           T1='WF10.WF10R3.WF1020',RF1=FB,LR1=42,                             
//*                                                                             
//           F2='WF10.WF10X1IN.WF1024',                                         
//           T2='WF10.WF10R3.WF1024',RF2=FB,LR2=42,                             
//*                                                                             
//           F3='WF10.WF10X1US.WF1028',                                         
//           T3='WF10.WF10R3.WF1028',RF3=FB,LR3=42,                             
//*                                                                             
//           F4='WF10.WF10X5SE.WF1040',                                         
//           T4='WF10.WF10R3.WF1040',RF4=FB,LR4=44                              
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10R3RS                                         
