//WB01J0X1 JOB (640WB010100WB01J0X1,W100),'RTN WB01X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//********************** VCOM SKAPA FIL ***********                             
//W016    EXEC W016P012,VCOMPARM=WB01X1                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=WB01.&VCOM..WB0120(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             DATACLAS=PSN,                                                    
//             DCB=(RECFM=FB,LRECL=493),                                        
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//********************** WAIT A SECOND FOR FILE 2 B CATALOGED                   
//WAIT    EXEC WWAIT,SECONDS=5                                                  
//*                                                                             
//********************** WB01 BESTÄLL RUTINEN *****                             
//SOP     EXEC WSOP                                                             
  ORDER WB01D1                                                                  
  ORDER WB01D1BLK                                                               
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WB01J0X1                                         
