//W371J24H JOB (640W3710100W371J24H,W100),'RTN W371D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*UPPFÖLJNING FÖR JAPAN OCH AUSTRALIEN SPECIAL FALL                            
//*AV RUTIN W371D3 OCH JOBB W371J04H                                            
//W371    EXEC W371P24H                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J24H                                         
