//WDF40DV1 JOB (650W0020200WDF40DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDF4    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDF4V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(16384,(1800,1800),RLSE),                                  
//             MGMTCLAS=EMID70                                                  
//DMP.WDF4V DD  DSN=WG01.QASE.WDF4V,DISP=SHR,DCB=BUFNO=22                       
//DMP.SYSIN DD  *                                                               
D1 WDF4     WDF4V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDF40DV1                                         
