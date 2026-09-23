//WDQ50DV1 JOB (650W0020200WDQ50DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDQ5  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDQ5K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(54000,1800),RLSE),                                  
//             MGMTCLAS=EMID70                                                  
//DMP.WDQ5K DD DSN=WG01.QASE.WDQ5K,DISP=SHR,                                    
//           AMP=('BUFND=13,BUFNI=10')                                          
//DMP.SYSIN  DD  *                                                              
D1 WDQ5     WDQ5K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDQ50DV1                                         
