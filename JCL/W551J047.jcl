//W551J047 JOB (650W5510100W551J047,W100),'RTN W551B1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W551    EXEC W551P047                                                         
//*                                                                             
//EMPTYT1 EXEC WEMPTST,DSIN=W551.W551B1.W55147(+1)                              
//*                                                                             
//    IF (EMPTYT1.T.RC = 4) THEN                                                
//*                                                                             
//* SKAPA 2 TOMFILER TILL ÅRETS FÖRSTA KÖRNING                                  
//*                                                                             
//REMPCR1 EXEC PGM=OPNCLOSE,PARM=W                                              
//WTOMFIL1  DD DSN=W551.W551B1.W55105(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             DCB=(RECFM=FB,LRECL=19,BLKSIZE=19),                              
//             DATACLAS=PSEN,                                                   
//             MGMTCLAS=NOBACKUP                                                
//REMPCR2 EXEC PGM=OPNCLOSE,PARM=W                                              
//WTOMFIL1  DD DSN=W551.W551B1.W55114(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             DCB=(RECFM=FB,LRECL=19,BLKSIZE=19),                              
//             DATACLAS=PSEN,                                                   
//             MGMTCLAS=NOBACKUP                                                
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551J047                                         
