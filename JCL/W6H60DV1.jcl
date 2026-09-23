//W6H60DV1 JOB (650W0020200W6H60DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W6H6  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.W6H6V(+1),DISP=(NEW,CATLG,DELETE),                
//             MGMTCLAS=EMID70,                                                 
//             SPACE=(CYL,(5,1),RLSE)                                           
//DMP.W6H6V DD DSN=WG01.QASE.W6H6V,DISP=SHR,                                    
//            DCB=BUFNO=22                                                      
//DMP.SYSIN  DD  *                                                              
D1 W6H6     W6H6V    OUTDD1                                                     
//*                                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W6H60DV1                                         
