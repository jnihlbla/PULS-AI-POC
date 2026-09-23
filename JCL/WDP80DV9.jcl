//WDP80DV9 JOB (650W0020200WDP80DV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WDP8    EXEC WG01DMP                                                          
//DMP.OUTDD1 DD DSN=WG01.DUMP.WDP8V(+1),                                        
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(4096,(19000,9000),RLSE),                                  
//             MGMTCLAS=EMID70,DATACLAS=MVOL                                    
//DMP.WDP8V DD DSN=WG01.QASE.WDP8V,DISP=SHR,                                    
//             DCB=BUFNO=8                                                      
//DMP.SYSIN  DD  *                                                              
D1 WDP8     WDP8V    OUTDD1                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WDP80DV9                                         
//*                                                                             
