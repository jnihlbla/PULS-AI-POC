//WDK90DV1 JOB (650W0020200WDK90DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDK9  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDK9V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(2048,(15000,315),RLSE),                                   
//             MGMTCLAS=EMID70                                                  
//DMP.WDK9V DD DSN=WG01.QASE.WDK9V,DISP=SHR,                                    
//            DCB=BUFNO=22                                                      
//DMP.SYSIN  DD  *                                                              
D1 WDK9     WDK9V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDK90DV1                                         
