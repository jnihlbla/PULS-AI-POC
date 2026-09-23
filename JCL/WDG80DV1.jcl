//WDG80DV1 JOB (650W0020200WDG80DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDG8    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDG8V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(16384,(27000,900),RLSE),                                  
//             MGMTCLAS=EMID70                                                  
//DMP.WDG8V DD  DSN=WG01.QASE.WDG8V,DISP=SHR,DCB=BUFNO=8                        
//DMP.SYSIN DD  *                                                               
D1 WDG8     WDG8V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDG80DV1                                         
