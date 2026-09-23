//WDR80DV9 JOB (650W0020200WDR80DV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDR8    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDR8K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(1800,1800),RLSE),                                   
//             MGMTCLAS=EMID70                                                  
//DMP.WDR8K  DD DSN=WG01.QASE.WDR8K,DISP=SHR,                                   
//             AMP=('BUFND=13,BUFNI=10')                                        
//DMP.SYSIN DD  *                                                               
D1 WDR8     WDR8K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR80DV9                                         
