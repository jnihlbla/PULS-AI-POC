//WDK80DV1 JOB (650W0020200WDK80DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDK8    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDK8K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(90000,1800),RLSE),                                  
//             MGMTCLAS=EMID35                                                  
//DMP.WDK8K DD  DSN=WG01.QASE.WDK8K,DISP=SHR,                                   
//            AMP=('BUFND=13,BUFNI=10')                                         
//DMP.SYSIN DD  *                                                               
D1 WDK8     WDK8K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDK80DV1                                         
