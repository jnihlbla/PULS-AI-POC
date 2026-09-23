//W335D1RS JOB (640W3350100W335D1RS,W100),'RTN W335D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W335D1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W335.W335X1M0.W33505',                                         
//           T1='W335.W335D1M0.W33505',RF1=VB,LR1=35,                           
//*                                                                             
//           F2='W335.W335X1M1.W33505',                                         
//           T2='W335.W335D1M1.W33505',RF2=VB,LR2=35,                           
//*                                                                             
//           F3='W335.W335X1M2.W33505',                                         
//           T3='W335.W335D1M2.W33505',RF3=VB,LR3=35,                           
//*                                                                             
//           F4='W335.W335X1M3.W33505',                                         
//           T4='W335.W335D1M3.W33505',RF4=VB,LR4=35,                           
//*                                                                             
//           F5='W335.W335X1M5.W33505',                                         
//           T5='W335.W335D1M5.W33505',RF5=VB,LR5=35                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335D1RS                                         
