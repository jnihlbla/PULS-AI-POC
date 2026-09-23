//W418J0X2 JOB (640W4180100W418J0X2,W100),'RTN W418X2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W418XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W418.&VCOM..W41802(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(33,(10,5),RLSE),AVGREC=K,                                 
//             DCB=(RECFM=VB,LRECL=33),                                         
//             MGMTCLAS=NOBACKUP                                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W418J0X2                                         
