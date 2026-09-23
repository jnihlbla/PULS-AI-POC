//WXTRJ069 JOB (640W0001000WXTRJ069,W100),'RTN WXTRV2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*  Skapar en "EXCEL"-fil med info om lagerplats på SDC24                      
//*  Nästa Job WXTRJM69 skickar denna som attachment i mail.                    
//*                                                                             
//WXTR    EXEC WXTRP069                                                         
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ069                                         
