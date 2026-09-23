//WDR50DV1 JOB (650W0020200WDR50DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDR5    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDR5V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(8192,(1080,90),RLSE),                                     
//             MGMTCLAS=EMID70                                                  
//DMP.WDR5V DD DSN=WG01.QASE.WDR5V,DISP=SHR,DCB=BUFNO=8                         
//DMP.SYSIN  DD  *                                                              
D1 WDR5     WDR5V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR50DV1                                         
