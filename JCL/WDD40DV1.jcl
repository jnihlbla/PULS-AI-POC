//WDD40DV1 JOB (650W0020200WDD40DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDD4    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDD4K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(180,180),RLSE),                                     
//             MGMTCLAS=EMID70                                                  
//DMP.WDD4K DD DSN=WG01.QASE.WDD4K,DISP=SHR,                                    
//             AMP=('BUFND=13,BUFNI=10')                                        
//DMP.SYSIN DD  *                                                               
D1 WDD4     WDD4K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDD40DV1                                         
