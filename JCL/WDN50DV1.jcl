//WDN50DV1 JOB (640W0020200WDN50DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*                                                                              
//WDN5    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDN5V(+1),DISP=(NEW,CATLG,DELETE),                
//             SPACE=(CYL,(100,10),RLSE),                                       
//             MGMTCLAS=EMID70                                                  
//DMP.WDN5V DD  DSN=WG01.QASE.WDN5V,DISP=SHR,DCB=BUFNO=8                        
//DMP.SYSIN DD  *                                                               
D1 WDN5     WDN5V    OUTDD1                                                     
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDN50DV1                                         
