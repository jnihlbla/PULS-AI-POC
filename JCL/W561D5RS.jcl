//W561D5RS JOB (640W5610100W561D5RS,W100),'RTN W561D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W561D5                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1=W561.W561D6.W56131,                                             
//           T1=W561.W561D5.W56131,RF1=FB,LR1=52                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W561D5RS                                         
