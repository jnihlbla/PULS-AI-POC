//W114J0X4 JOB (640W1140100W114J0X4,W100),'RTN W114X4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W114X4                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W114.&VCOM..PI75R7H1(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             DCB=(RECFM=FB,LRECL=70),                                         
//             MGMTCLAS=NOBACKUP,DATACLAS=PSEN                                  
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//SOP     EXEC WSOP                                                             
  ORDER W114D4                                                                  
  END-ORDER                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114J0X4                                         
