//WXTRV2RS JOB (650W0001000WXTRV2RS,W100),'RTN WXTRV2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=WXTRV2                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='WXTR.WXTRD1.WXTR03',                                           
//           T1='WXTR.WXTRV2.WXTR03',RF1=FB,LR1=084,                            
//*                                                                             
//           F2='WXTR.WXTRD1.WXTR04',                                           
//           T2='WXTR.WXTRV2.WXTR04',RF2=FB,LR2=037,                            
//*                                                                             
//           F3='WXTR.WXTRD1.WXTR05',                                           
//           T3='WXTR.WXTRV2.WXTR05',RF3=FB,LR3=202,                            
//*                                                                             
//           F4='WXTR.WXTRD1.WXTR07',                                           
//           T4='WXTR.WXTRV2.WXTR07',RF4=FB,LR4=074,                            
//*                                                                             
//           F5='WXTR.WXTRD1.WXTRA0',                                           
//           T5='WXTR.WXTRV2.WXTRA0',RF5=FB,LR5=142,                            
//*                                                                             
//           F6='WXTR.WXTRD1.WXTRA5',                                           
//           T6='WXTR.WXTRV2.WXTRA5',RF6=FB,LR6=395,                            
//*                                                                             
//           F7='W221.W221S2.W22104A',                                          
//           T7='W221.WXTRV2.W22104A',RF7=FB,LR7=190,                           
//*                                                                             
//           F8='W221.W221S2.W22105A',                                          
//           T8='W221.WXTRV2.W22105A',RF8=FB,LR8=190                            
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WXTRV2RS                                         
