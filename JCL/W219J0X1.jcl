//W219J0X1 JOB (640W2190100W219J0X1,W100),'RTN W219X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W219X1                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
W219X1SE                                                                        
//*                                                                             
//W01612.W016XXD1 DD DSN=W219.W219X1SE.W21901(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             DATACLAS=PSEB,                                                   
//             DCB=(RECFM=FB,LRECL=72),                                         
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//SOP     EXEC WSOP                                                             
  ORDER W219S1                                                                  
  END-ORDER                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W219J0X1                                         
