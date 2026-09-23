//WDR90DV1 JOB (650W0020200WDR90DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDR9  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDR9K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(126000,18000),RLSE),                                
//             MGMTCLAS=EMID70                                                  
//DMP.WDR9K DD DSN=WG01.QASE.WDR9K,DISP=SHR,                                    
//           AMP=('BUFND=13,BUFNI=10')                                          
//DMP.SYSIN  DD  *                                                              
D1 WDR9     WDR9K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR90DV1                                         
