//WDN10DV1 JOB (640W0020200WDN10DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDN1    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDN1V(+1),DISP=(NEW,CATLG,DELETE),                
//             SPACE=(CYL,(1,1),RLSE),                                          
//             MGMTCLAS=EMID70                                                  
//DMP.WDN1V DD  DSN=WG01.QASE.WDN1V,DISP=SHR,DCB=BUFNO=22                       
//DMP.SYSIN DD  *                                                               
D1 WDN1     WDN1V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDN10DV1                                         
