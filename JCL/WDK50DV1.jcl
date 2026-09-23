//WDK50DV1 JOB (650W0020200WDK50DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDK5    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDK5K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(180,180),RLSE),                                     
//             MGMTCLAS=EMID70                                                  
//DMP.WDK5K DD  DSN=WG01.QASE.WDK5K,DISP=SHR,                                   
//            AMP=('BUFND=13,BUFNI=10')                                         
//DMP.SYSIN DD  *                                                               
D1 WDK5     WDK5K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDK50DV1                                         
