//WDL60DV9 JOB (650W0020200WDL60DV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDL6    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDL6V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(8192,(250000,27000),RLSE),                                
//             MGMTCLAS=EMID70,DATACLAS=MVOL                                    
//DMP.WDL6V DD DSN=WG01.QASE.WDL6V,DISP=SHR,                                    
//             DCB=BUFNO=8                                                      
//DMP.SYSIN  DD  *                                                              
D1 WDL6     WDL6V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL60DV9                                         
//*                                                                             
