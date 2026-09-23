//WDC10DV1 JOB (650W0020200WDC10DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDC1    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDC1V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(2048,(42525,315),RLSE),                                   
//             MGMTCLAS=EMID70                                                  
//DMP.WDC1V DD  DSN=WG01.QASE.WDC1V,DISP=SHR,                                   
//             DCB=BUFNO=22                                                     
//DMP.SYSIN DD  *                                                               
D1 WDC1     WDC1V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDC10DV1                                         
