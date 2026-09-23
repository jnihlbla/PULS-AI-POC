//W6G30DV1 JOB (650W0020200W6G30DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W6G3  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.W6G3K(+1),                                        
//           DISP=(NEW,CATLG,DELETE),MGMTCLAS=EMID70,                           
//           SPACE=(4096,(5500,550),RLSE)                                       
//DMP.W6G3K DD DSN=WG01.QASE.W6G3K,DISP=SHR,                                    
//           AMP=('BUFND=13,BUFNI=10')                                          
//DMP.SYSIN  DD  *                                                              
D1 W6G3     W6G3K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W6G30DV1                                         
