//WDN40DV1 JOB (640W0020200WDN40DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDN4    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDN4V(+1),DISP=(NEW,CATLG,DELETE),                
//             SPACE=(CYL,(12,2),RLSE),                                         
//             MGMTCLAS=EMID70                                                  
//DMP.WDN4V DD  DSN=WG01.QASE.WDN4V,DISP=SHR,DCB=BUFNO=22                       
//DMP.SYSIN DD  *                                                               
D1 WDN4     WDN4V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDN40DV1                                         
