//W092J0X4 JOB (640W0920100W092J0X4,W100),'RTN W092X4',                         
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
&VCOM                                                                           
//*                                                                             
//*THIS VCOM JOB IS NOT IN USE. CAN BE DELETED ALONG WITH ITS                   
//*VCOM COMPONENTS                                                              
//W01612.W016XXD1 DD DSN=W.CC.W092X4PP.EPIC(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             DATACLAS=PSEB,                                                   
//             DCB=(RECFM=VB,LRECL=86),                                         
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W092J0X4                                         
