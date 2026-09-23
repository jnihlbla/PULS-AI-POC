//W611J0X1 JOB (640W6110100W611J0X1,W100),'RTN W611X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W611XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
W611X1SE                                                                        
//*                                                                             
//W01612.W016XXD1 DD DSN=W611.W611X1SE.W61101(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             DATACLAS=PSEB,                                                   
//             DCB=(RECFM=FB,LRECL=1009),                                       
//             MGMTCLAS=NOBACKUP                                                
//*                                                                             
//ORDER   EXEC WSOP                                                             
  ORDER W611S1 SYMBOLS                                                          
    VCOM(W611X1SE)                                                              
  END-ORDER                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J0X1                                         
