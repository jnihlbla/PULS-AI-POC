//WDQ40DV9 JOB (650W0020200WDQ40DV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDQ4    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDQ4K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(54000,3600),RLSE),                                  
//             MGMTCLAS=EMID70                                                  
//DMP.WDQ4K DD DSN=WG01.QASE.WDQ4K,DISP=SHR,                                    
//             DCB=BUFNO=13                                                     
//DMP.SYSIN  DD  *                                                              
D1 WDQ4     WDQ4K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDQ40DV9                                         
