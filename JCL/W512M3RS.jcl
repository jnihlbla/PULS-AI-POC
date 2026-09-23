//W512M3RS JOB (640W5120100W512M3RS,W100),'RTN W512M3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W512M3                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//             F1=W570.W570D2.W57066B,                                          
//             T1=W570.W512M3.W57066B,RF1=FB,LR1=292,                           
//*                                                                             
//             F2=W515.W515D2.W51566B,                                          
//             T2=W515.W512M3.W51566B,RF2=FB,LR2=292,                           
//*                                                                             
//             F3=W570.W570D2.W5706BC,                                          
//             T3=W570.W512M3.W5706BC,RF3=FB,LR3=292,                           
//*                                                                             
//             F4=W515.W515D2.W5156BC,                                          
//             T4=W515.W512M3.W5156BC,RF4=FB,LR4=292,                           
//*                                                                             
//             F5=W570.W570D3.W57069B,                                          
//             T5=W570.W512M3.W57069B,RF5=FB,LR5=61,                            
//*                                                                             
//             F6=W515.W515D3.W51569B,                                          
//             T6=W515.W512M3.W51569B,RF6=FB,LR6=61,                            
//*                                                                             
//             F7=W561.W561D2.W5616BA,                                          
//             T7=W561.W512M3.W5616BA,RF7=FB,LR7=292                            
//*                                                                             
//NEWGEN1 EXEC PGM=OPNCLOSE,PARM=DD                                             
//DD1      DD  DSN=W570.W570D2.W57066B(+1),DISP=(NEW,CATLG,DELETE),             
//             RECFM=FB,LRECL=292,                                              
//             DATACLAS=PSEN,MGMTCLAS=BACKUPC                                   
//*                                                                             
//NEWGEN2 EXEC PGM=OPNCLOSE,PARM=DD                                             
//DD1      DD  DSN=W515.W515D2.W51566B(+1),DISP=(NEW,CATLG,DELETE),             
//             RECFM=FB,LRECL=292,                                              
//             DATACLAS=PSEN,MGMTCLAS=BACKUPC                                   
//*                                                                             
//NEWGEN3 EXEC PGM=OPNCLOSE,PARM=DD                                             
//DD1      DD  DSN=W570.W570D3.W57069B(+1),DISP=(NEW,CATLG,DELETE),             
//             RECFM=FB,LRECL=61,                                               
//             DATACLAS=PSEN,MGMTCLAS=BACKUPC                                   
//*                                                                             
//NEWGEN4 EXEC PGM=OPNCLOSE,PARM=DD                                             
//DD1      DD  DSN=W515.W515D3.W51569B(+1),DISP=(NEW,CATLG,DELETE),             
//             RECFM=FB,LRECL=61,                                               
//             DATACLAS=PSEN,MGMTCLAS=BACKUPC                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W512M3RS                                         
