//W371J063 JOB (640W3710100W371J063,W100),'RTN W371V4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*AFTER MEMOAPIX                                                                
//*---  WMEMOSND,EXC                                                            
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//********************************************************************          
//*                                                                  *          
//*    OM FIL W37164 INNEHÅLLER POSTER SKICKAS ETT MEMO TILL         *          
//*    PRICE-MANAGER FÖR BYTES-SYSTEMET                              *          
//*                                                                  *          
//********************************************************************          
//EMPTYT1 EXEC WEMPTST,DSIN=W371.W371V4.W37163(+0)                              
//*                                                                             
//    IF (EMPTYT1.T.RC = 0) THEN                                                
//W371   EXEC WMEMOSND                                                          
//M.APIFILE DD DSN=W371.W371V4.W37165(+0),DISP=SHR                              
//          DD DSN=W371.W371V4.W37163(+0),DISP=SHR                              
//          DD DSN=W.QASE.CONSTANT(W371V4ME),DISP=SHR                           
//M.SEND    DD DUMMY                                                            
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J063                                         
