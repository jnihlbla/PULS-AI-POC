//WDJ70DV9 JOB (650W0020200WDJ70DV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDJ7    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDJ7K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(18000,1800),RLSE),                                  
//             MGMTCLAS=EMID70                                                  
//DMP.WDJ7K DD DSN=WG01.QASE.WDJ7K,DISP=SHR,                                    
//            DCB=BUFNO=13                                                      
//DMP.SYSIN  DD  *                                                              
D1 WDJ7     WDJ7K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDJ70DV9                                         
