//WDQ20DV9 JOB (650W0020200WDQ20DV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDQ2    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDQ2V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(360000,18000),RLSE),                                
//             MGMTCLAS=EMID70,DATACLAS=MVOL                                    
//DMP.WDQ2V DD DSN=WG01.QASE.WDQ2V,DISP=SHR,                                    
//             DCB=BUFNO=8                                                      
//DMP.SYSIN  DD  *                                                              
D1 WDQ2     WDQ2V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDQ20DV9                                         
//*                                                                             
