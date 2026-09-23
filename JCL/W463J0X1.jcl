//W463J0X1 JOB (640W4630100W463J0X1,W100),'RTN W463X1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
/*CNTL  W46381,EXC                                                              
//*                                                                             
//* -- TEST PÅ OM DET REDAN FINNS 255 GENERATIONER                              
//* -- STOPPA SKAPANDANDET AV NYA OCH KOLLA VARFÖR!                             
//MAXTST  EXEC WGENRTST,DSIN=W463.&VCOM..W46381,GEN=255                         
//MAXIF   IF (MAXTST.T.RC = 0) THEN                                             
//ABEND   EXEC PGM=ABEND,PARM='ANVÄNDAR-ABEND 17'                               
//MAXIF   ENDIF                                                                 
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W463XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W463.&VCOM..W46381(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(53,(10,5),RLSE),AVGREC=K,                                 
//             DCB=(RECFM=FB,LRECL=53),                                         
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//ORDER   EXEC WSOP                                                             
                    ORDER W463S6 SYMBOLS                                        
                    VCOM(&VCOM)                                                 
                    END-ORDER                                                   
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463J0X1                                         
