//WDP70DV1 JOB (650W0020200WDP70DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDP7    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDP7V(+1),DISP=(NEW,CATLG,DELETE),                
//             SPACE=(4096,(6000,360),RLSE),                                    
//             MGMTCLAS=EMID70                                                  
//DMP.WDP7V  DD DSN=WG01.QASE.WDP7V,DISP=SHR,                                   
//             DCB=BUFNO=13                                                     
//DMP.SYSIN DD  *                                                               
D1 WDP7     WDP7V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDP70DV1                                         
