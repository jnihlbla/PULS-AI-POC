//W114J0X7 JOB (640W1140100W114J0X7,W100),'RTN W114X7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W114X7                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W114.&VCOM..T335R309(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             DCB=(RECFM=FB,LRECL=580),                                        
//             MGMTCLAS=NOBACKUP,DATACLAS=PSEN                                  
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//SOP     EXEC WSOP                                                             
  ORDER W114S7                                                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114J0X7                                         
