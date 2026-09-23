//W6F10DV1 JOB (650W0020200W6F10DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W6F1  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.W6F1V(+1),DISP=(NEW,CATLG,DELETE),                
//             MGMTCLAS=EMID70,                                                 
//             SPACE=(CYL,(9,1),RLSE)                                           
//DMP.W6F1V DD DSN=WG01.QASE.W6F1V,DISP=SHR,                                    
//            DCB=BUFNO=8                                                       
//DMP.SYSIN  DD  *                                                              
D1 W6F1     W6F1V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W6F10DV1                                         
