//WDC20DV1 JOB (650W0020200WDC20DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDC2    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDC2V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(8192,(180,90),RLSE),                                      
//             MGMTCLAS=EMID70                                                  
//DMP.WDC2V DD  DSN=WG01.QASE.WDC2V,DISP=SHR,                                   
//             DCB=BUFNO=8                                                      
//DMP.SYSIN DD  *                                                               
D1 WDC2     WDC2V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDC20DV1                                         
