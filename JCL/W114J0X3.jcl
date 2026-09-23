//W114J0X3 JOB (640W1140100W114J0X3,W100),'RTN W114X3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//********************** VCOM SKAPA FIL ***********                             
//W016    EXEC W016P012,VCOMPARM=W114XX                                         
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//W01612.W016XXD1 DD DSN=W.MY.&VCOM..W11410(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             DCB=(RECFM=FB,LRECL=300),                                        
//             MGMTCLAS=BACKUPC,DATACLAS=PSEN                                   
//*                                                                             
//* SOPORDER GÖRS EJ DÅ W114D3 STARTAS PÅ KLOCKSLAG 06:00                       
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W114J0X3                                         
