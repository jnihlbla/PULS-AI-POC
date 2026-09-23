//WDM40DV1 JOB (650W0020200WDM40DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDM4  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDM4V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(2048,(315,315),RLSE),                                     
//             MGMTCLAS=EMID70                                                  
//DMP.WDM4V DD DSN=WG01.QASE.WDM4V,DISP=SHR,                                    
//            DCB=BUFNO=22                                                      
//DMP.SYSIN  DD  *                                                              
D1 WDM4     WDM4V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDM40DV1                                         
