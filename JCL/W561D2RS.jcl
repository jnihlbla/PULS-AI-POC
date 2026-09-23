//W561D2RS JOB (650W5100100W561D2RS,W100),'RTN W561D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W561D2                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W510.W510D2.W51043',                                           
//           T1='W510.W561D2.W51043',RF1=FB,LR1=265,CP1=15,                     
//*                                                                             
//           F2='W510.W510D2.W5104C',                                           
//           T2='W510.W561D2.W5104C',RF2=FB,LR2=265,CP2=15,                     
//*                                                                             
//           F3='W426.W426V1.W42637',                                           
//           T3='W426.W561D2.W42637',RF3=FB,LR3=265,CP3=15,                     
//*                                                                             
//           F4='W476.W476D5.W4768G',                                           
//           T4='W476.W561D2.W4768G',RF4=FB,LR4=265,CP4=15                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W561D2RS                                         
