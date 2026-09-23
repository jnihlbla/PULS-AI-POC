//WDK10DV9 JOB (650W0020200WDK10DV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDK1  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDK1V(+1),                                        
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(4096,(59400,1800),RLSE),                                    
//           MGMTCLAS=EMID70                                                    
//DMP.WDK1V DD DSN=WG01.QASE.WDK1V,DISP=SHR,                                    
//           DCB=BUFNO=13                                                       
//DMP.SYSIN  DD  *                                                              
D1 WDK1     WDK1V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDK10DV9                                         
