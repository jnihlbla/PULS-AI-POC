//WDC30DV1 JOB (650W0020200WDC30DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDC3    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDC3V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(2048,(31500,315),RLSE),                                   
//             MGMTCLAS=EMID70                                                  
//DMP.WDC3V DD  DSN=WG01.QASE.WDC3V,DISP=SHR,                                   
//             DCB=BUFNO=22                                                     
//DMP.SYSIN DD  *                                                               
D1 WDC3     WDC3V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDC30DV1                                         
