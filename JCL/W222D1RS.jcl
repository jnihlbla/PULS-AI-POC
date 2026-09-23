//W222D1RS JOB (640W2220100W222D1RS,W100),'RTN W222D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W222D1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W092.W092D2.W09257',                                           
//           T1='W092.W222D1.W09257',RF1=VB,LR1=038,                            
//*                                                                             
//           F2='W111.W111D1.W11126',                                           
//           T2='W111.W222D1.W11126',RF2=FB,LR2=13,                             
//*                                                                             
//           F3='W222.DUMMY.W22201',                                            
//           T3='W222.W222D1.W22201',RF3=FB,LR3=10,                             
//*                                                                             
//           F4='W222.DUMMY.W22202',                                            
//           T4='W222.W222D1.W22202',RF4=FB,LR4=18,                             
//*                                                                             
//           F5='W222.DUMMY.W22203',                                            
//           T5='W222.W222D1.W22203',RF5=FB,LR5=28,                             
//*                                                                             
//           F6='W222.DUMMY.W22214',                                            
//           T6='W222.W222D1.W22214',RF6=FB,LR6=18,                             
//*                                                                             
//           F7='W222.DUMMY.W22215',                                            
//           T7='W222.W222D1.W22215',RF7=FB,LR7=28                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W222D1RS                                         
