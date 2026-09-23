//W330J0X1 JOB (650W3300100W330J0X1,W100),'RTN W330X1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM LINES=9,CARDS=0,FORMS=1800                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W330XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W330.&VCOM..W33033(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(84,(100,10),RLSE),AVGREC=K,                               
//             DCB=(RECFM=VB,LRECL=84),                                         
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W330J0X1                                         
