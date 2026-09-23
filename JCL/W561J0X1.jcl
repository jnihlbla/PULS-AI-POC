//W561J0X1 JOB (670W5100100W561J0X1,W100),'RTN W561X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W561XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=WIN.&VCOM..W56112(+1),                                 
//             DISP=(NEW,CATLG,DELETE),                                         
//             DATACLAS=PSEB,                                                   
//             DCB=(RECFM=VB,LRECL=1054),                                       
//             MGMTCLAS=NOBACKUP                                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W561J0X1                                         
