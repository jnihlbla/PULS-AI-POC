//WDD20DV1 JOB (650W0020200WDD20DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDD2    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDD2V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(450000,3150),RLSE),                                 
//             MGMTCLAS=EMID70                                                  
//DMP.WDD2V DD  DSN=WG01.QASE.WDD2V,DISP=SHR,DCB=BUFNO=22                       
//DMP.SYSIN DD  *                                                               
D1 WDD2     WDD2V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDD20DV1                                         
