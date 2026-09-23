//W611V1RS JOB (640W6110100W611V1RS,W100),'RTN W611V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W611V1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W611.W611D2.W61128',                                           
//           T1='W611.W611V1.W61128',RF1=FB,LR1=046,                            
//*                                                                             
//           F2='W611.W611D1.W61151',                                           
//           T2='W611.W611V1.W61151',RF2=FB,LR2=053,                            
//*                                                                             
//           F3='W611.W611D2.W61184',                                           
//           T3='W611.W611V1.W61184',RF3=FB,LR3=027,                            
//*                                                                             
//           F4='W612.W612V8.W6121G',                                           
//           T4='W612.W611V1.W6121G',RF4=FB,LR4=020                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611V1RS                                         
