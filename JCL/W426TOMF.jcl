//W426TOMF JOB (640W4260100W426TOMF,W100),'RTN W426Y1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//ALLOC    EXEC PGM=OPNCLOSE,PARM='R426'                                        
//W426   DD DSN=W426.W426V4.W42670(+1),                                         
//           DISP=(NEW,CATLG,DELETE),                                           
//           LRECL=293,RECFM=VB,BLKSIZE=27998,                                  
//           SPACE=(TRK,(1,1)),                                                 
//           MGMTCLAS=BACKUP7                                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W426TOMF                                         
