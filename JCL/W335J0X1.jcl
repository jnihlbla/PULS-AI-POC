//W335J0X1 JOB (670W3350100W335J0X1,W100),'RTN W335X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W335XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W335.&VCOM..W33501(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(24,(100,10),RLSE),AVGREC=K,                               
//             DCB=(RECFM=VB,LRECL=27),                                         
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//*ORDER   EXEC WSOP,COMMAND='ORDER W335D1'                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W335J0X1                                         
