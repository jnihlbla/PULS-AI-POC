//WDH50DV1 JOB (650W0020200WDH50DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDH5  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDH5V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(2048,(126000,18000),RLSE),                                
//             MGMTCLAS=EMID70                                                  
//DMP.WDH5V DD DSN=WG01.QASE.WDH5V,DISP=SHR,                                    
//           DCB=BUFNO=13                                                       
//DMP.SYSIN  DD  *                                                              
D1 WDH5     WDH5V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDH50DV1                                         
