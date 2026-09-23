//W479M1RS JOB (670W4790100W479M1RS,W100),'RTN W479M1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W479M1                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1=W479.W479V5.W47935,                                             
//           T1=W479.W479M1.W47935,RF1=F,LR1=0028                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479M1RS                                         
