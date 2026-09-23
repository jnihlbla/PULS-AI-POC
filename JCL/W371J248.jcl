//W371J248 JOB (640W3710100W371J248,W100),'RTN W371D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*UPPFÖLJNING FÖR JAPAN OCH AUSTRALIEN SPECIAL FALL                            
//*AV RUTIN W371D3 OCH JOBB W371J048                                            
//W371    EXEC W371P248                                                         
//*                                                                             
//W37148.W37148D1 DD  DSN=W371.W371D4.W3714B(+0),DISP=SHR                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J248                                         
