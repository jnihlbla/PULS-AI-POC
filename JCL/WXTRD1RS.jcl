//WXTRD1RS JOB (650W0001000WXTRD1RS,W100),'RTN WXTRD1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=WXTRD1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W225.W225D2.W22502',                                           
//           T1='W225.WXTRD1.W22502',RF1=FB,LR1=30,                             
//*                                                                             
//           F2='W479.W479D1.W47923B',                                          
//           T2='W479.WXTRD1.W47923B',RF2=FB,LR2=320,                           
//*                                                                             
//           F3='W479.W479D1.W47925X',                                          
//           T3='W479.WXTRD1.W47925X',RF3=FB,LR3=91,                            
//*                                                                             
//           F4='W479.W479D2.W4795N',                                           
//           T4='W479.WXTRD1.W4795N',RF4=FB,LR4=168,                            
//*                                                                             
//           F5='W479.W479D1.W4795O',                                           
//           T5='W479.WXTRD1.W4795O',RF5=FB,LR5=088,                            
//*                                                                             
//           F6='W479.W479D1.W47986',                                           
//           T6='W479.WXTRD1.W47986',RF6=FB,LR6=190,                            
//*                                                                             
//           F7='W479.W479D1.W47990',                                           
//           T7='W479.WXTRD1.W47990',RF7=FB,LR7=011                             
//SOP     EXEC WSOPEND,PROCESS=WXTRD1RS                                         
