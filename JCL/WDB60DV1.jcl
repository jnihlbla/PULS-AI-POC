//WDB60DV1 JOB (650W0020200WDB60DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDB6    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDB6V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(2048,(315,315),RLSE),                                     
//             MGMTCLAS=EMID70                                                  
//DMP.WDB6V DD  DSN=WG01.QASE.WDB6V,DISP=SHR,DCB=BUFNO=13                       
//DMP.SYSIN DD  *                                                               
D1 WDB6     WDB6V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDB60DV1                                         
