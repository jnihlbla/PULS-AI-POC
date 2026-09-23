//W213J0X1 JOB (640W2130100W213J0X1,W100),'RTN W213X1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W213XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W.MY.W213X1SE.A31481(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             DATACLAS=PSEN,                                                   
//             DCB=(RECFM=FB,LRECL=426),                                        
//             MGMTCLAS=NOBACKUP                                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W213J0X1                                         
