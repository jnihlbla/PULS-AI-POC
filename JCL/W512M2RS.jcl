//W512M2RS JOB (640W5120100W512M2RS,W100),'RTN W512M2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W512M2                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1=W216.W216S1.W21612,                                             
//           T1=W216.W512M2.W21612,RF1=FB,LR1=1537                              
//*                                                                             
//NEWGEN  EXEC PGM=OPNCLOSE,PARM=DD                                             
//DD1      DD  DSN=W216.W216S1.W21612(+1),DISP=(NEW,CATLG,DELETE),              
//             RECFM=FB,LRECL=1537,                                             
//             DATACLAS=PSEN,MGMTCLAS=BACKUPC                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W512M2RS                                         
