//WDE60DV1 JOB (650W0020200WDE60DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDE6    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDE6V(+1),DISP=(NEW,CATLG,DELETE),                
//             SPACE=(4096,(216000,5400),RLSE),                                 
//             MGMTCLAS=EMID70                                                  
//DMP.WDE6V DD  DSN=WG01.QASE.WDE6V,DISP=SHR,DCB=BUFNO=13                       
//DMP.SYSIN DD  *                                                               
D1 WDE6     WDE6V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDE60DV1                                         
