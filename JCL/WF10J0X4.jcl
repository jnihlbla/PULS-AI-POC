//WF10J0X4 JOB (640WF100100WF10J0X4,W100),'RTN WF10X4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=WF10X1                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=WF10.&VCOM..WF1028(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             DATACLAS=PSEN,                                                   
//             DCB=(RECFM=FB,LRECL=42),                                         
//             MGMTCLAS=NOBACKUP                                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WF10J0X4                                         
