//W517V1RS JOB (650W5170100W517V1RS,W100),'RTN W517V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W517V1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W510.W510D1.W51011',                                           
//           T1='W510.W517V1.W51011',RF1=FB,LR1=430,CP1=15,                     
//*                                                                             
//           F2='W517.W510D1.W51711',                                           
//           T2='W517.W517V1.W51711',RF2=VB,LR2=44,                             
//*                                                                             
//           F3='W510.W510D4.W51714',                                           
//           T3='W517.W517V1.W51714',RF3=VB,LR3=44,                             
//*                                                                             
//           F4='W611.W611V1.W61164',                                           
//           T4='W611.W517V1.W61164',RF4=FB,LR4=27,                             
//*                                                                             
//           F5='W612.W612V3.W61270',                                           
//           T5='W612.W517V1.W61270',RF5=FB,LR5=27,                             
//*                                                                             
//           F6='W570.W570D1.W57011',                                           
//           T6='W570.W517V1.W57011',RF6=FB,LR6=430,                            
//*                                                                             
//           F7='W570.W570D1.W51711',                                           
//           T7='W570.W517V1.W51711',RF7=VB,LR7=44                              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W517V1RS                                         
