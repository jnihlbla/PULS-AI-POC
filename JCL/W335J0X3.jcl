//W335J0X3 JOB (670W3350100W335J0X3,W100),'RTN W335X3',                         
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
//W01612.W016XXD1 DD DSN=W335.&VCOM..W33520(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(25,(100,10),RLSE),AVGREC=K,                               
//             DCB=(RECFM=VB,LRECL=25),                                         
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//*ORDER   EXEC WSOP,COMMAND='ORDER W335D2'                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W335J0X3                                         
