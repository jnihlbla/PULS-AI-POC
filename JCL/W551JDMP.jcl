//W551JDMP JOB (670W5510100W551JDMP,W100),'RTN W551B1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//WDC6    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDC6V(+1),                                        
//           DISP=(NEW,CATLG,DELETE),MGMTCLAS=EMID70,                           
//           SPACE=(2048,(12600,1260),RLSE)                                     
//DMP.WDC6V DD DSN=WG01.QASE.WDC6V,DISP=SHR,                                    
//           DCB=BUFNO=22                                                       
//DMP.SYSIN  DD   *                                                             
D1 WDC6     WDC6V    OUTDD1                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551JDMP                                         
