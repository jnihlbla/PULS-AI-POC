//WDE90DV1 JOB (650W0020200WDE90DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDE9    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDE9K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             MGMTCLAS=EMID70,                                                 
//             SPACE=(4096,(1800,180),RLSE)                                     
//DMP.WDE9K DD DSN=WG01.QASE.WDE9K,DISP=SHR,                                    
//             AMP=('BUFND=13,BUFNI=10')                                        
//DMP.SYSIN  DD  *                                                              
D1 WDE9     WDE9K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDE90DV1                                         
