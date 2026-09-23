//W6G10DV1 JOB (650W0020200W6G10DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W6G1  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.W6G1V(+1),DISP=(NEW,CATLG,DELETE),                
//             MGMTCLAS=EMID70,                                                 
//             SPACE=(CYL,(1,1),RLSE)                                           
//DMP.W6G1V DD DSN=WG01.QASE.W6G1V,DISP=SHR,                                    
//            DCB=BUFNO=8                                                       
//DMP.SYSIN  DD  *                                                              
D1 W6G1     W6G1V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W6G10DV1                                         
