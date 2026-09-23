//WDT40DV1 JOB (650W0020200WDT40DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDT4    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDT4K(+1),DISP=(NEW,CATLG,DELETE),                
//             SPACE=(4096,(1800,180),RLSE),                                    
//             MGMTCLAS=EMID70                                                  
//DMP.WDT4K  DD DSN=WG01.QASE.WDT4K,DISP=SHR,DCB=BUFNO=13                       
//DMP.SYSIN DD  *                                                               
D1 WDT4     WDT4K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDT40DV1                                         
