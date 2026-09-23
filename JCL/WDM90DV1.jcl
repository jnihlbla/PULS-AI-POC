//WDM90DV1 JOB (640W0020200WDM90DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDM9    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDM9V(+1),DISP=(NEW,CATLG,DELETE),                
//             SPACE=(4096,(18000,180),RLSE),                                   
//             MGMTCLAS=EMID70                                                  
//DMP.WDM9V DD  DSN=WG01.QASE.WDM9V,DISP=SHR,DCB=BUFNO=8                        
//DMP.SYSIN DD  *                                                               
D1 WDM9     WDM9V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDM90DV1                                         
