//WDD4ICV9 JOB (650W0020200WDD4ICV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDD4    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDD4K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             MGMTCLAS=EMID70,                                                 
//             SPACE=(4096,(1800,1800),RLSE)                                    
//DMP.WDD4K DD DSN=WG01.QASE.WDD4K,DISP=SHR,                                    
//            DCB=BUFNO=13                                                      
//DMP.SYSIN  DD  *                                                              
D1 WDD4     WDD4K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDD4ICV9                                         
