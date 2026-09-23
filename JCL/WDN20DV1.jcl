//WDN20DV1 JOB (640W0020200WDN20DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDN2    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDN2V(+1),DISP=(NEW,CATLG,DELETE),                
//             SPACE=(CYL,(3,1),RLSE),                                          
//             MGMTCLAS=EMID70                                                  
//DMP.WDN2V DD  DSN=WG01.QASE.WDN2V,DISP=SHR,DCB=BUFNO=22                       
//DMP.SYSIN DD  *                                                               
D1 WDN2     WDN2V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDN20DV1                                         
