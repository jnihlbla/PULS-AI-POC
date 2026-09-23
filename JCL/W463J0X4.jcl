//W463J0X4 JOB (640W4630100W463J0X4,W100),'RTN W463X4',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
/*CNTL W46355,EXC                                                               
//*                                                                             
//* -- TEST PÅ OM DET REDAN FINNS 255 GENERATIONER                              
//* -- KOLLA VARFÖR!                                                            
//MAXTST  EXEC WGENRTST,DSIN=WIN.&VCOM..W46355,GEN=255                          
//MAXIF   IF (MAXTST.T.RC = 0 ) THEN                                            
//ABEND   EXEC PGM=ABEND,PARM='ANVÄNDAR-ABEND 17'                               
//MAXIF   ENDIF                                                                 
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W463XY                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=WIN.&VCOM..W46355(+1),                                 
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(1009,(10,5),RLSE),AVGREC=K,                               
//             DCB=(RECFM=VB,LRECL=1009),                                       
//             MGMTCLAS=NOBACKUP                                                
//*                                                                             
//* -- ISF ATT STARTA W463S9 VARJE GÅNG EN W463X4SE STARTAS                     
//* -- TRIGGAR VI W463S9 FOM JAN 2019 VIA W980HOUR                              
//* -- MEN OM FÖRMODAN SKULLE DET KOMMA MER ÄN 200 FILER INOM 30 MIN            
//* -- GÖR VI EN EXTRA ORDER PÅ W463S9 SÅ ATT VI INTE NÅR 255-GRÄNSEN           
//GENTST  EXEC WGENRTST,DSIN=WIN.&VCOM..W46355,GEN=200                          
//GENIF   IF (GENTST.T.RC = 0 ) THEN  -----------------------                   
//ORDER   EXEC WSOP                                                             
  ORDER W463S9 ON &ON  SYMBOLS                                                  
    VCOM(&VCOM)                                                                 
  END-ORDER                                                                     
//GENIF   ENDIF                      -----------------------                    
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463J0X4                                         
