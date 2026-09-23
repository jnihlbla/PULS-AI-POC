//WF23S1RS JOB (640WF210100WF23S1RS,W100),'RTN WF23S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=WF23S1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='WF20.WF20S2.WF2031',                                           
//           T1='WF20.WF23S1.WF2031',RF1=VB,LR1=1,                              
//           X1='LRECL(5699)',CP1=155                                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF23S1RS                                         
