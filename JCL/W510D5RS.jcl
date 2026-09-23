//W510D5RS JOB (650W5100100W510D5RS,W100),'RTN W510D5',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W510D5                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W510.W510D4.W51074',                                           
//           T1='W510.W510D5.W51074',RF1=VB,LR1=755,CP1=10,                     
//*                                                                             
//           F2='W510.W510D4.W5107O',                                           
//           T2='W510.W510D5.W5107O',RF2=FB,LR2=121,CP2=10,                     
//*                                                                             
//           F3='W510.W510D4.W5107P',                                           
//           T3='W510.W510D5.W5107P',RF3=FB,LR3=121,CP3=10,                     
//*                                                                             
//           F4='W510.W510D4.W51082',                                           
//           T4='W510.W510D5.W51082',RF4=FB,LR4=121,CP4=10,                     
//*                                                                             
//           F5='W510.W510D2.W5106M',                                           
//           T5='W510.W510D5.W5106M',RF5=VB,LR5=1003,CP5=10                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W510D5RS                                         
