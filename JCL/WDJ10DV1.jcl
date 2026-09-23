//WDJ10DV1 JOB (650W0020200WDJ10DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDJ1    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDJ1V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(3870,180),RLSE),                                    
//             MGMTCLAS=EMID70                                                  
//DMP.WDJ1V DD  DSN=WG01.QASE.WDJ1V,DISP=SHR,DCB=BUFNO=13                       
//DMP.SYSIN DD  *                                                               
D1 WDJ1     WDJ1V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDJ10DV1                                         
