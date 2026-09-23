//W335D4RS JOB (640W3350100W335D4RS,W100),'RTN W335D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W335D4                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W335.W335X5M0.W33508',                                         
//           T1='W335.W335D4M0.W33508',RF1=VB,LR1=911,                          
//*                                                                             
//           F2='W335.W335X5M1.W33508',                                         
//           T2='W335.W335D4M1.W33508',RF2=VB,LR2=911,                          
//*                                                                             
//           F3='W335.W335X5M2.W33508',                                         
//           T3='W335.W335D4M2.W33508',RF3=VB,LR3=911,                          
//*                                                                             
//           F4='W335.W335X5M3.W33508',                                         
//           T4='W335.W335D4M3.W33508',RF4=VB,LR4=911,                          
//*                                                                             
//           F5='W335.W335X5M5.W33508',                                         
//           T5='W335.W335D4M5.W33508',RF5=VB,LR5=911                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335D4RS                                         
