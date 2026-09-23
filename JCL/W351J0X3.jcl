//W351J0X3 JOB (640W3510100W351J0X3,W100),'RTN W351X3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W351XX                                         
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W351.&VCOM..W35111(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(84,(1000,500),RLSE),AVGREC=K,                             
//             DCB=(RECFM=VB,LRECL=84),                                         
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W351J0X3                                         
