//W261J0X1 JOB (670W2610100W261J0X1,W100),'RTN W261X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W261XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W261.&VCOM..W26164(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(31,(100,10),RLSE),AVGREC=K,                               
//             DCB=(RECFM=FB,LRECL=31),                                         
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W261B5                                                                  
  END-ORDER                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W261J0X1                                         
