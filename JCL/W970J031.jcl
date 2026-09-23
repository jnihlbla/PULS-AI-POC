//W970J031 JOB (640W0000100W970J031,W100),'RTN W970B3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ   NJESD                                                            
/*ROUTE  PRINT NJOVC                                                            
//*                                                                             
//W970 EXEC  W001ISPF                                                           
//SYSTSIN DD *                                                                  
 ISPSTART CMD(%W970RULD)                                                        
//TSO.INPUT    DD DSN=W.QASE.CONSTANT(W970&ACLGRP),DISP=SHR                     
//TSO.ACF2RULE DD DSN=W970.W970B3.ACF2RULE,DISP=SHR                             
//TSO.REPORT   DD DSN=W970.W970B3.W97030(+1),                                   
//             DISP=(MOD,CATLG,DELETE),RECFM=VBA,LRECL=137,                     
//             MGMTCLAS=DEL2,DATACLAS=PSEN                                      
//TSO.OLDDD    DD DSN=&&OLDDD,                                                  
//             DISP=(NEW,DELETE),RECFM=VBA,                                     
//             DATACLAS=PSEN,VOL=(,,,1)                                         
//TSO.NEWDD    DD DSN=&&NEWDD,                                                  
//             DISP=(NEW,DELETE),RECFM=VBA,                                     
//             DATACLAS=PSEN,VOL=(,,,1)                                         
//TSO.OUTDD    DD DSN=&&OUTDD,                                                  
//             DISP=(NEW,DELETE),RECFM=VBA,                                     
//             DATACLAS=PSEN,VOL=(,,,1)                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W970J031                                         
