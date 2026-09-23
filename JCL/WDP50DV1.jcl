//WDP50DV1 JOB (650W0020200WDP50DV1,W100),'RTN W010V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDP5  EXEC WG01DMP                                                            
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDP5V(+1),DISP=(NEW,CATLG,DELETE),                
//             MGMTCLAS=EMID70,                                                 
//             SPACE=(CYL,(120,20),RLSE)                                        
//DMP.WDP5V DD DSN=WG01.QASE.WDP5V,DISP=SHR,                                    
//            DCB=BUFNO=13                                                      
//DMP.SYSIN  DD  *                                                              
D1 WDP5     WDP5V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDP50DV1                                         
