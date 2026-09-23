//WDL5ICV9 JOB (640W0020200WDL5ICV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDL5    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDL5V(+1),DISP=(NEW,CATLG,DELETE),                
//             SPACE=(16384,(90000,9000),RLSE),                                 
//             MGMTCLAS=EMID70                                                  
//DMP.WDL5V DD  DSN=WG01.QASE.WDL5V,DISP=SHR,DCB=BUFNO=8                        
//DMP.SYSIN DD  *                                                               
D1 WDL5     WDL5V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL5ICV9                                         
