//W111J0X1 JOB (640W1110100W111J0X1,W100),'RTN W111X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W111XX                                         
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W.SG.&VCOM..W11137(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             DATACLAS=PSEN,                                                   
//             DCB=(RECFM=VB,LRECL=255),                                        
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W111J0X1                                         
