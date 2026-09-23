//WDA90DV1 JOB (650W0020200WDA90DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDA9    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDA9V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(5040,360),RLSE),                                    
//             MGMTCLAS=EMID70,DATACLAS=MVOL                                    
//DMP.WDA9V DD DSN=WG01.QASE.WDA9V,DISP=SHR,                                    
//            DCB=BUFNO=8                                                       
//DMP.SYSIN  DD  *                                                              
D1 WDA9     WDA9V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDA90DV1                                         
//*                                                                             
