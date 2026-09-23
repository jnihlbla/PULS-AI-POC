//WDM70DV1 JOB (640W0020200WDM70DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*                                                                              
//WDM7  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDM7K(+1),                                        
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(4096,(900,90),RLSE),                                        
//           MGMTCLAS=EMID70                                                    
//DMP.WDM7K DD DSN=WG01.QASE.WDM7K,DISP=SHR,                                    
//           AMP=('BUFND=13,BUFNI=10')                                          
//DMP.SYSIN  DD  *                                                              
D1 WDM7     WDM7K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDM70DV1                                         
