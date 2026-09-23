//W114J0X2 JOB (640W1140100W114J0X2,W100),'RTN W114X2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W114XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W114.&VCOM..W11424(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             DCB=(RECFM=FB,LRECL=100),                                        
//             MGMTCLAS=NOBACKUP,DATACLAS=PSEN                                  
//*                                                                             
//SOP    EXEC WSOP                                                              
  ORDER W114S9                                                                  
  END-ORDER                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W114J0X2                                         
