//WDL90DV1 JOB (650W0020200WDL90DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDL9    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDL9K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(144000,9000),RLSE),                                 
//             MGMTCLAS=EMID70,DATACLAS=MVOL                                    
//DMP.WDL9K DD DSN=WG01.QASE.WDL9K,DISP=SHR,                                    
//             AMP=('BUFND=13,BUFNI=10')                                        
//DMP.SYSIN  DD  *                                                              
D1 WDL9     WDL9K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL90DV1                                         
//*                                                                             
