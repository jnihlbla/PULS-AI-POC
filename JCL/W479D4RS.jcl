//W479D4RS JOB (640W4790100W479D4RS,W100),'RTN W479D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W479D4                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1=W479.W479D1.W47923,                                             
//           T1=W479.W479D4.W47923,RF1=FB,LR1=0101                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479D4RS                                         
