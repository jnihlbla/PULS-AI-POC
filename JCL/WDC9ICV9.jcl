//WDC9ICV9 JOB (650W0020200WDC9ICV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDC9    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDC9K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             MGMTCLAS=EMID70,                                                 
//             SPACE=(4096,(1800,1800),RLSE)                                    
//DMP.WDC9K DD DSN=WG01.QASE.WDC9K,DISP=SHR,                                    
//            DCB=BUFNO=13                                                      
//DMP.SYSIN  DD  *                                                              
D1 WDC9     WDC9K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDC9ICV9                                         
