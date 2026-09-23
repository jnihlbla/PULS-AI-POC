//WDA2ICV9 JOB (640W0020200WDA2ICV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDA2    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDA2V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(8192,(220000,9000),RLSE),                                 
//             MGMTCLAS=EMID70                                                  
//DMP.WDA2V  DD  DSN=WG01.QASE.WDA2V,DISP=SHR,                                  
//           DCB=BUFNO=13                                                       
//DMP.SYSIN DD *                                                                
D1 WDA2     WDA2V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDA2ICV9                                         
