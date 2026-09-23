//W114J0X1 JOB (640W1140100W114J0X1,W100),'RTN W114X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W114X1                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W.SG.W114X1SE.W11412(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             DATACLAS=PSEB,                                                   
//             DCB=(RECFM=FB,LRECL=338),                                        
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//* //ORDER  EXEC WSOP                                                          
//*     ORDER W114S8                                                            
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W114J0X1                                         
