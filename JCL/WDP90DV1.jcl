//WDP90DV1 JOB (650W0020200WDP90DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDP9    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDP9K(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(1800,1800),RLSE),                                   
//             MGMTCLAS=EMID70,DATACLAS=MVOL                                    
//DMP.WDP9K DD  DSN=WG01.QASE.WDP9K,DISP=SHR,DCB=BUFNO=13                       
//DMP.SYSIN DD  *                                                               
D1 WDP9     WDP9K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDP90DV1                                         
