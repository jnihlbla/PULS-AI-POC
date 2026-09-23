//WDR2ICV9 JOB (640W0020200WDR2ICV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDR2    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDR2V(+1),DISP=(NEW,CATLG,DELETE),                
//             SPACE=(8192,(6300,180),RLSE),                                    
//             MGMTCLAS=EMID70                                                  
//DMP.WDR2V DD  DSN=WG01.QASE.WDR2V,DISP=SHR,DCB=BUFNO=8                        
//DMP.SYSIN DD  *                                                               
D1 WDR2     WDR2V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR2ICV9                                         
