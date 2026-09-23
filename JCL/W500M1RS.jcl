//W500M1RS JOB (650W5100100W500M1RS,W100),'RTN W500M1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W500M1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W510.W510D4.W51076',                                           
//           T1='W510.W500M1.W51076',RF1=FB,LR1=24,                             
//*                                                                             
//           F2='W517.W510D1.W51709',                                           
//           T2='W517.W500M1.W51709',RF2=FB,LR2=22,                             
//*                                                                             
//           F3='W510.W510D4.W51713',                                           
//           T3='W517.W500M1.W51713',RF3=FB,LR3=22,                             
//*                                                                             
//           F4='W512.W512X1SE.W51211',                                         
//           T4='WIN.W500M1.A11079',RF4=FB,LR4=42,                              
//*                                                                             
//           F5='W510.W510D4.W5106G',                                           
//           T5='W510.W500M1.W5106G',RF5=FB,LR5=57,                             
//*                                                                             
//           F6='W570.W570D1.W51709',                                           
//           T6='W570.W500M1.W51709',RF6=FB,LR6=22,                             
//*                                                                             
//           F7='W510.W510D4.W51066A',                                          
//           T7='W510.W500M1.W51066A',RF7=FB,LR7=310                            
//*                                                                             
//NEWGEN1 EXEC PGM=OPNCLOSE,PARM=DD                                             
//DD1      DD  DSN=W510.W510D4.W51066A(+1),DISP=(NEW,CATLG,DELETE),             
//             RECFM=FB,LRECL=310,                                              
//             DATACLAS=PSEN,MGMTCLAS=BACKUPC                                   
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W500M1RS                                         
