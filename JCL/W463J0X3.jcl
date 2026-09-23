//W463J0X3 JOB (640W4630100W463J0X3,W100),'RTN W463X3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W463XY                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=WIN.&VCOM..W46383(+1),                                 
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(1009,(10,5),RLSE),AVGREC=K,                               
//             DCB=(RECFM=VB,LRECL=1009),                                       
//             MGMTCLAS=NOBACKUP                                                
//*                                                                             
//GENTST  EXEC WGENRTST,DSIN=WIN.&VCOM..W46383,GEN=1                            
//GENIF   IF (GENTST.T.RC = 0 ) THEN  -----------------------                   
//ORDER   EXEC WSOP                                                             
                    ORDER W463S8 SYMBOLS                                        
                    VCOM(&VCOM)                                                 
                    END-ORDER                                                   
//GENIF   ENDIF                      -----------------------                    
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463J0X3                                         
