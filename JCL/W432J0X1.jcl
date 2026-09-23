//W432J0X1 JOB (670W4320100W432J0X1,W100),'RTN W432X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W432XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W432.&VCOM..W43218(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(299,(100,10),RLSE),AVGREC=K,                              
//             DCB=(RECFM=FB,LRECL=299),                                        
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W432J0X1                                         
