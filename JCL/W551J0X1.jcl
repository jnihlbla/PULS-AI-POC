//W551J0X1 JOB (640W5510100W551J0X1,W100),'RTN W551X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*                                                                             
//*  ---------- "SOP SYMBOL VALUES" för denna körning ------------              
//*                                                                             
//* VCOM(&VCOM) MCOMP(&MCOMP)                                                   
//*                                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W551XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//*                                                                             
//W01612.W016XXD1 DD DSN=W551.W551X1.W551X1(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             DCB=(RECFM=VB,LRECL=6),                                          
//             MGMTCLAS=BACKUPC,DATACLAS=PSEN                                   
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W551B6BLK                                                               
  ORDER W551B6 SYMBOLS                                                          
    MCOMP(&MCOMP)                                                               
  END-ORDER                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551J0X1                                         
