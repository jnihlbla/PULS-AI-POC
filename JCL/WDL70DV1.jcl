//WDL70DV1 JOB (650W0020200WDL70DD1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDL7    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDL7V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(8192,(384000,20000),RLSE),                                
//             DATACLAS=MVOL,                                                   
//             MGMTCLAS=EMID70                                                  
//DMP.WDL7V DD DSN=WG01.QASE.WDL7V,DISP=SHR,                                    
//             DCB=BUFNO=13                                                     
//DMP.SYSIN DD  *                                                               
D1 WDL7     WDL7V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL70DV1                                         
