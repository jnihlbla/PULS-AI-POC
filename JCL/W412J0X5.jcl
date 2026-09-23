//W412J0X5 JOB (640W4120100W412J0X5,W100),'RTN W412X5',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W412XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W412.IN.W41210(+1), -LIM(10)                           
//             DISP=(NEW,CATLG,DELETE),                                         
//             DATACLAS=PSEN,                                                   
//             DCB=(RECFM=VB,LRECL=334),                                        
//             MGMTCLAS=NOBACKUP                                                
//*                                                                             
//ORDER   EXEC WSOP                                                             
                    ORDER W412S1 SYMBOLS                                        
                      ORDTYP(W412X5)                                            
                    END-ORDER                                                   
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W412J0X5                                         
