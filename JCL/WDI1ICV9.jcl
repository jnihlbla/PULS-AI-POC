//WDI1ICV9 JOB (640W0020200WDI1ICV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDI1    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDI1V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(36000,1800),RLSE),                                  
//             MGMTCLAS=EMID70                                                  
//DMP.WDI1V  DD  DSN=WG01.QASE.WDI1V,DISP=SHR,                                  
//           DCB=BUFNO=13                                                       
//DMP.SYSIN DD *                                                                
D1 WDI1     WDI1V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDI1ICV9                                         
