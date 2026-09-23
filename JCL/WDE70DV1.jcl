//WDE70DV1 JOB (650W0020200WDE70DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDE7    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDE7V(+1),DISP=(NEW,CATLG,DELETE),                
//             SPACE=(4096,(72000,3600),RLSE),                                  
//             MGMTCLAS=EMID70                                                  
//DMP.WDE7V DD  DSN=WG01.QASE.WDE7V,DISP=SHR,DCB=BUFNO=13                       
//DMP.SYSIN DD  *                                                               
D1 WDE7     WDE7V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDE70DV1                                         
