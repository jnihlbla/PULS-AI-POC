//WDK20DV1 JOB (650W0020200WDK20DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDK2  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDK2V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(3600,180),RLSE),                                    
//             MGMTCLAS=EMID70                                                  
//DMP.WDK2V DD DSN=WG01.QASE.WDK2V,DISP=SHR,                                    
//            DCB=BUFNO=22                                                      
//DMP.SYSIN  DD  *                                                              
D1 WDK2     WDK2V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDK20DV1                                         
