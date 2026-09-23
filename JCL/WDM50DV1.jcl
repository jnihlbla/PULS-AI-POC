//WDM50DV1 JOB (640W0020200WDM50DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDM5    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDM5V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(2048,(315,315),RLSE),                                     
//             MGMTCLAS=EMID70                                                  
//DMP.WDM5V DD  DSN=WG01.QASE.WDM5V,DISP=SHR,DCB=BUFNO=13                       
//DMP.SYSIN DD  *                                                               
D1 WDM5     WDM5V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDM50DV1                                         
