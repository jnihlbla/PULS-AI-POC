//W271D9RS JOB (640W2710100W271D9RS,W100),'RTN W271D9',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W271D9                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W271.W271V1.W27138',                                           
//           T1='W271.W271D9.W27138V1',RF1=FB,LR1=047,                          
//*                                                                             
//           F2='W271.W271D2.W27137',                                           
//           T2='W271.W271D9.W27137D2',RF2=FB,LR2=018,                          
//*                                                                             
//           F3='W271.W271DB.W27137',                                           
//           T3='W271.W271D9.W27137DB',RF3=FB,LR3=018,                          
//*                                                                             
//           F4='W271.W271V2.W27137',                                           
//           T4='W271.W271D9.W27137V2',RF4=FB,LR4=018,                          
//*                                                                             
//           F5='W271.W271D2.W27135',                                           
//           T5='W271.W271D9.W27135D2',RF5=FB,LR5=012,                          
//*                                                                             
//           F6='W271.W271DB.W27135',                                           
//           T6='W271.W271D9.W27135DB',RF6=FB,LR6=012,                          
//*                                                                             
//           F7='W271.W271V2.W27135',                                           
//           T7='W271.W271D9.W27135V2',RF7=FB,LR7=012                           
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME02 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W271.W271V1.W2713A',                                           
//           T1='W271.W271D9.W2713A',RF1=FB,LR1=047,                            
//*                                                                             
//           F2='W271.W271D8.W27103',                                           
//           T2='W271.W271D9.W27103',RF2=FB,LR2=035,                            
//*                                                                             
//           F3='W271.W271V1.W2713F',                                           
//           T3='W271.W271D9.W2713FV1',RF3=FB,LR3=047,                          
//*                                                                             
//           F4='W271.W271V6.W27131',                                           
//           T4='W271.W271D9.W27131V6',RF4=FB,LR4=018                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271D9RS                                         
