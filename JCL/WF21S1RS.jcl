//WF21S1RS JOB (640WF210100WF21S1RS,W100),'RTN WF21S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=WF21S1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='WF20.WF20S2.WF2011',                                           
//           T1='WF20.WF21S1.WF2011',RF1=VB,LR1=1,                              
//           X1='LRECL(5699)',CP1=155,                                          
//*                                                                             
//           F2='WF20.WF20S2.WF2012',                                           
//           T2='WF20.WF21S1.WF2012',RF2=FB,LR2=0316,CP2=155,                   
//*                                                                             
//           F3='WF20.WF20S2.WF2013',                                           
//           T3='WF20.WF21S1.WF2013',RF3=FB,LR3=0184,CP3=155,                   
//*                                                                             
//           F4='WF20.WF20S2.WF2014',                                           
//           T4='WF20.WF21S1.WF2014',RF4=FB,LR4=0459,CP4=155,                   
//*                                                                             
//           F5='WF20.WF20S2.WF2017',                                           
//           T5='WF20.WF21S1.WF2017',RF5=FB,LR5=1642,CP5=155,                   
//*                                                                             
//           F6='WF20.WF20S2.WF2018',                                           
//           T6='WF20.WF21S1.WF2018',RF6=FB,LR6=0248,CP6=155,                   
//*                                                                             
//           F7='WF20.WF20S2.WF2037',                                           
//           T7='WF20.WF21S1.WF2037',RF7=FB,LR7=1642,CP7=155,                   
//*                                                                             
//           F8='WF20.WF20S2.WF2031A',                                          
//           T8='WF20.WF21S1.WF2031A',RF8=FB,LR8=1,                             
//           X8='LRECL(5699)',CP8=155                                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF21S1RS                                         
