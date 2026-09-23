//W271J0X1 JOB (640W2710100W271J0X1,W100),'RTN W271X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W271XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W271.W271X1.W27122(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(123,(1,1),RLSE),AVGREC=U,                                 
//             DCB=(RECFM=FB,LRECL=123),                                        
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//SOP     EXEC WSOP                                                             
  ORDER W271BJ                                                                  
  END-ORDER                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W271J0X1                                         
