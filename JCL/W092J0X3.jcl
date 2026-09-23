//W092J0X3 JOB (640W0920100W092J0X3,W100),'RTN W092X3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W092XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
W092X3SE                                                                        
//*                                                                             
//W01612.W016XXD1 DD DSN=W.MY.W092X3SE.TIURPROD(+1),                            
//             DISP=(NEW,CATLG,DELETE),                                         
//             DATACLAS=PSEB,                                                   
//             DCB=(RECFM=FB,LRECL=47),                                         
//             MGMTCLAS=NOBACKUP                                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W092J0X3                                         
