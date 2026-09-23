//WDL8ICV9 JOB (640W0020200WDL8ICV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDL8    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDL8V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(16384,(162000,9900),RLSE),                                
//             MGMTCLAS=EMID70,DATACLAS=MVOL                                    
//DMP.WDL8V  DD  DSN=WG01.QASE.WDL8V,DISP=SHR,                                  
//           DCB=BUFNO=7                                                        
//DMP.SYSIN DD *                                                                
D1 WDL8     WDL8V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDL8ICV9                                         
