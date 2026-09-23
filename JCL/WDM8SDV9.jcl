//WDM8SDV9 JOB (640W0020200WDM8SDV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*                                                                              
//WDM8  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDM8K(+1),                                        
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(4096,(900,90),RLSE),                                        
//           MGMTCLAS=EMID70                                                    
//DMP.WDM8K DD DSN=WG01.QASE.WDM8K,DISP=SHR,                                    
//           AMP=('BUFND=13,BUFNI=10')                                          
//DMP.SYSIN  DD  *                                                              
D1 WDM8     WDM8K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDM8SDV9                                         
