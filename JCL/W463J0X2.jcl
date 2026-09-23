//W463J0X2 JOB (640W4630100W463J0X2,W100),'RTN W463X2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* -- TEST PÅ OM DET REDAN FINNS 255 GENERATIONER                              
//* -- STOPPA SKAPANDET AV NYA, OCH KOLLA VARFÖR!                               
//MAXTST  EXEC WGENRTST,DSIN=W463.&VCOM..W46334,GEN=255                         
//MAXIF   IF (MAXTST.T.RC = 0) THEN                                             
//ABEND   EXEC PGM=ABEND,PARM='ANVÄNDAR-ABEND 17'                               
//MAXIF   ENDIF                                                                 
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W463XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W463.&VCOM..W46334(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             DCB=(RECFM=FB,LRECL=128),                                        
//             MGMTCLAS=BACKUPC,DATACLAS=PSEN                                   
//*                                                                             
//ORDER   EXEC WSOP                                                             
    ORDER W463S5 ON &ON SYMBOLS                                                 
       VCOM(&VCOM)                                                              
    END-ORDER                                                                   
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463J0X2                                         
