//WDH1ICV9 JOB (640W0020200WDH1ICV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDH1    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDH1V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(2048,(1980,630),RLSE),                                    
//             MGMTCLAS=EMID70                                                  
//DMP.WDH1V  DD  DSN=WG01.QASE.WDH1V,DISP=SHR,                                  
//           DCB=BUFNO=13                                                       
//DMP.SYSIN DD *                                                                
D1 WDH1     WDH1V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDH1ICV9                                         
