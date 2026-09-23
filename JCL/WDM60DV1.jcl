//WDM60DV1 JOB (650W0020200WDM60DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDM6    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDM6V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(2048,(31500,3150),RLSE),                                  
//             MGMTCLAS=EMID70                                                  
//DMP.WDM6V DD  DSN=WG01.QASE.WDM6V,DISP=SHR,DCB=BUFNO=22                       
//DMP.SYSIN DD  *                                                               
D1 WDM6     WDM6V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDM60DV1                                         
