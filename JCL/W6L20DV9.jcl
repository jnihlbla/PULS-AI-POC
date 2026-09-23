//W6L20DV9 JOB (650W0020200W6L20DV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W6L2  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.W6L2K(+1),                                        
//           DISP=(NEW,CATLG,DELETE),MGMTCLAS=EMID70,                           
//           SPACE=(4096,(45000,9000),RLSE)                                     
//DMP.W6L2K DD DSN=WG01.QASE.W6L2K,DISP=SHR,                                    
//           AMP=('BUFND=13,BUFNI=10')                                          
//DMP.SYSIN  DD  *                                                              
D1 W6L2     W6L2K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W6L20DV9                                         
