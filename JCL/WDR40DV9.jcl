//WDR40DV9 JOB (650W0020200WDR40DV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDR4    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDR4V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(8192,(3600,360),RLSE),                                    
//             MGMTCLAS=EMID70,DATACLAS=MVOL                                    
//DMP.WDR4V DD DSN=WG01.QASE.WDR4V,DISP=SHR,                                    
//             DCB=BUFNO=8                                                      
//DMP.SYSIN  DD  *                                                              
D1 WDR4     WDR4V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR40DV9                                         
//*                                                                             
