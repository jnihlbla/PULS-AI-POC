//WDJ30DV1 JOB (650W0020200WDJ30DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDJ3    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDJ3K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(1800,180),RLSE),                                    
//             MGMTCLAS=EMID70                                                  
//DMP.WDJ3K  DD DSN=WG01.QASE.WDJ3K,DISP=SHR,                                   
//           AMP=('BUFND=13,BUFNI=10')                                          
//DMP.SYSIN DD  *                                                               
D1 WDJ3     WDJ3K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDJ30DV1                                         
