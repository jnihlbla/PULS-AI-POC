//WDR10DV1 JOB (650W0020200WDR10DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDR1  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDR1V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(8192,(5400,1900),RLSE),                                   
//             MGMTCLAS=EMID70                                                  
//DMP.WDR1V DD DSN=WG01.QASE.WDR1V,DISP=SHR,                                    
//            DCB=BUFNO=8                                                       
//DMP.SYSIN  DD  *                                                              
D1 WDR1     WDR1V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDR10DV1                                         
