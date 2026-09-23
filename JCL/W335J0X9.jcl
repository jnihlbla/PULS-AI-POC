//W335J0X9 JOB (670W3350100W335J0X9,W100),'RTN W335X9',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*     VCOMPARM = &VCOM                                                        
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W335XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W335.&VCOM..W33581(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(11,(1,1),RLSE),AVGREC=U,                                  
//             DCB=(RECFM=FB,LRECL=11),                                         
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W335J0X9                                         
