//WDA50DV1 JOB (650W0020200WDA50DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDA5    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDA5K(+1),DISP=(NEW,CATLG,DELETE),                
//             SPACE=(4096,(18000,1800),RLSE),                                  
//             MGMTCLAS=EMID70                                                  
//DMP.WDA5K  DD DSN=WG01.QASE.WDA5K,DISP=SHR,                                   
//             AMP=('BUFND=13,BUFNI=10')                                        
//DMP.SYSIN DD  *                                                               
D1 WDA5     WDA5K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDA50DV1                                         
