//WDL20DV9 JOB (650W0020200WDL20DV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDL2    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDL2V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),DCB=(BLKSIZE=27998,BUFNO=5),             
//             SPACE=(2048,(252000,12600),RLSE),                                
//             MGMTCLAS=EMID70,DATACLAS=MVOL                                    
//DMP.WDL2V DD DSN=WG01.QASE.WDL2V,DISP=SHR,                                    
//             DCB=BUFNO=22                                                     
//DMP.SYSIN  DD  *                                                              
D1 WDL2     WDL2V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL20DV9                                         
//*                                                                             
