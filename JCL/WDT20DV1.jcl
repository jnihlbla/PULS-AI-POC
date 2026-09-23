//WDT20DV1 JOB (650W0020200WDT20DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDT2    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDT2V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(1800,180),RLSE),                                    
//             MGMTCLAS=EMID70                                                  
//DMP.WDT2V  DD  DSN=WG01.QASE.WDT2V,DISP=SHR,                                  
//           DCB=BUFNO=13                                                       
//DMP.SYSIN DD *                                                                
D1 WDT2     WDT2V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDT20DV1                                         
