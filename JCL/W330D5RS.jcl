//W330D5RS JOB (650W3300100W330D5RS,W100),'RTN W330D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W330D5                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W330.W330D1.W33009',                                           
//           T1='W330.W330D5.W33009',RF1=VB,LR1=40,CP1=15,                      
//*                                                                             
//           F2='W371.W371V1.W3710G',                                           
//           T2='W371.W330D5.W3710G',RF2=FB,LR2=26,CP2=15,                      
//*                                                                             
//           F3='W418.W418D2.W41839',                                           
//           T3='W418.W330D5.W41839',RF3=VB,LR3=30,CP3=15,                      
//*                                                                             
//           F4='W476.W476D5.W47654',                                           
//           T4='W476.W330D5.W47654',RF4=FB,LR4=26,CP4=15                       
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W330D5RS                                         
