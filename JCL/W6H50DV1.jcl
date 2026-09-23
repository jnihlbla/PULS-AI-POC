//W6H50DV1 JOB (650W0020200W6H50DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W6H5  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.W6H5K(+1),DISP=(NEW,CATLG,DELETE),                
//             MGMTCLAS=EMID70,                                                 
//             SPACE=(CYL,(1,1),RLSE)                                           
//DMP.W6H5K DD DSN=WG01.QASE.W6H5K,DISP=SHR,                                    
//            AMP=('BUFND=34,BUFNI=10')                                         
//DMP.SYSIN  DD  *                                                              
D1 W6H5     W6H5K    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W6H50DV1                                         
