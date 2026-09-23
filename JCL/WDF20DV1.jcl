//WDF20DV1 JOB (650W0020200WDF20DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDF2    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDF2V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(16384,(1800,1800),RLSE),                                  
//             MGMTCLAS=EMID70                                                  
//DMP.WDF2V DD  DSN=WG01.QASE.WDF2V,DISP=SHR,DCB=BUFNO=22                       
//DMP.SYSIN DD  *                                                               
D1 WDF2     WDF2V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDF20DV1                                         
