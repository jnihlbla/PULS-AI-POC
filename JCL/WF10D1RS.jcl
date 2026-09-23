//WF10D1RS JOB (640WF100100WF10D1RS,W100),'RTN WF10D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=WF10D1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W510.W510D1.W51013',                                           
//           T1='W510.WF10D1.W51013',RF1=VB,LR1=1050,                           
//*                                                                             
//           F2='W570.W570D1.W57013',                                           
//           T2='W570.WF10D1.W57013',RF2=VB,LR2=1050,                           
//*                                                                             
//           F3='W515.W515D1.W51513',                                           
//           T3='W515.WF10D1.W51513',RF3=VB,LR3=1050,                           
//*                                                                             
//           F4='W561.W561D1.W56113',                                           
//           T4='W561.WF10D1.W56113',RF4=FB,LR4=1050,                           
//*                                                                             
//           F5='WF10.WF10D4.WF1053',                                           
//           T5='WF10.WF10D1.WF1053',RF5=FB,LR5=1054                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10D1RS                                         
