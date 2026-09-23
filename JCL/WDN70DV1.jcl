//WDN70DV1 JOB (640W0020200WDN70DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDN7    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDN7V(+1),DISP=(NEW,CATLG,DELETE),                
//             SPACE=(CYL,(8,1),RLSE),                                          
//             MGMTCLAS=EMID70                                                  
//DMP.WDN7V DD  DSN=WG01.QASE.WDN7V,DISP=SHR,DCB=BUFNO=22                       
//DMP.SYSIN DD  *                                                               
D1 WDN7     WDN7V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDN70DV1                                         
