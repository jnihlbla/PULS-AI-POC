//WDB40DV1 JOB (650W0020200WDB40DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDB4    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDB4V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(360,180),RLSE),                                     
//             MGMTCLAS=EMID70                                                  
//DMP.WDB4V  DD  DSN=WG01.QASE.WDB4V,DISP=SHR,DCB=BUFNO=13                      
//DMP.SYSIN DD *                                                                
D1 WDB4     WDB4V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDB40DV1                                         
