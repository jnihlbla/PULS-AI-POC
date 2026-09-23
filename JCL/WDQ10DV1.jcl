//WDQ10DV1 JOB (650W0020200WDQ10DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDQ1  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDQ1K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(126000,18000),RLSE),                                
//             MGMTCLAS=EMID70                                                  
//DMP.WDQ1K DD DSN=WG01.QASE.WDQ1K,DISP=SHR,                                    
//           AMP=('BUFND=13,BUFNI=10')                                          
//DMP.SYSIN  DD  *                                                              
D1 WDQ1     WDQ1K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDQ10DV1                                         
