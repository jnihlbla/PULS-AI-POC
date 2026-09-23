//W161J0X2 JOB (640W1610100W161J0X2,W100),'RTN W161X2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W161XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=&W161..&VCOM..W16152(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             DCB=(RECFM=FB,LRECL=256),                                        
//             MGMTCLAS=NOBACKUP,DATACLAS=PSEN                                  
//*                                                                             
//SOP    EXEC WSOP                                                              
  ORDER W161S2                                                                  
  END-ORDER                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W161J0X2                                         
