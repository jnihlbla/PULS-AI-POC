//WF10J0X2 JOB (640WF100100WF10J0X2,W100),'RTN WF10X2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=WF10X1                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=WF10.&VCOM..WF1020(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             DATACLAS=PSEN,                                                   
//             DCB=(RECFM=FB,LRECL=42),                                         
//             MGMTCLAS=NOBACKUP                                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WF10J0X2                                         
