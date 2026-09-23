//W335D2RS JOB (640W3350100W335D2RS,W100),'RTN W335D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W335D2                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W335.W335X3M0.W33506',                                         
//           T1='W335.W335D2M0.W33506',RF1=VB,LR1=33,                           
//*                                                                             
//           F2='W335.W335X3M1.W33506',                                         
//           T2='W335.W335D2M1.W33506',RF2=VB,LR2=33,                           
//*                                                                             
//           F3='W335.W335X3M2.W33506',                                         
//           T3='W335.W335D2M2.W33506',RF3=VB,LR3=33,                           
//*                                                                             
//           F4='W335.W335X3M3.W33506',                                         
//           T4='W335.W335D2M3.W33506',RF4=VB,LR4=33,                           
//*                                                                             
//           F5='W335.W335X3M5.W33506',                                         
//           T5='W335.W335D2M5.W33506',RF5=VB,LR5=33                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335D2RS                                         
