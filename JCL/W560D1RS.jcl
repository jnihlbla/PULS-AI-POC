//W560D1RS JOB (650W5600100W560D1RS,W100),'RTN W560D1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W560D1                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W510.W510D2.W51031',                                           
//           T1='W510.W560D1.W51031',RF1=VB,LR1=256,CP1=15,                     
//*                                                                             
//           F2='W510.W510D2.W51033',                                           
//           T2='W510.W560D1.W51033',RF2=VB,LR2=147,CP2=15,                     
//*                                                                             
//           F3='W510.W510D2.W51040',                                           
//           T3='W510.W560D1.W51040',RF3=VB,LR3=158,CP3=15,                     
//*                                                                             
//           F4='W513.W510D2.W5134N',                                           
//           T4='W513.W560D1.W5134N',RF4=FB,LR4=076,                            
//*                                                                             
//           F5='W513.W510D2.W5134P',                                           
//           T5='W513.W560D1.W5134P',RF5=FB,LR5=080                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W560D1RS                                         
