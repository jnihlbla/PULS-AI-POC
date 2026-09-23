//WDB50DV1 JOB (650W0020200WDB50DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDB5  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDB5K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             MGMTCLAS=EMID70,                                                 
//             SPACE=(4096,(540,180),RLSE)                                      
//DMP.WDB5K DD DSN=WG01.QASE.WDB5K,DISP=SHR,                                    
//            DCB=BUFNO=13                                                      
//DMP.SYSIN  DD  *                                                              
D1 WDB5     WDB5K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDB50DV1                                         
