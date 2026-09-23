//W330V1RS JOB (650W3300100W330V1RS,W100),'RTN W330V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W330V1                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W330.W330D5.W33009C',                                          
//           T1='W330.W330V1.W33009',RF1=VB,LR1=40,                             
//*                                                                             
//           F2='W371.W330D5.W3710GC',                                          
//           T2='W371.W330V1.W3710G',RF2=FB,LR2=26,                             
//*                                                                             
//           F3='W418.W330D5.W41839C',                                          
//           T3='W418.W330V1.W41839',RF3=VB,LR3=30,                             
//*                                                                             
//           F4='W476.W330D5.W47654C',                                          
//           T4='W476.W330V1.W47654',RF4=FB,LR4=26                              
//SOP     EXEC WSOPEND,PROCESS=W330V1RS                                         
