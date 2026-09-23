//W980JPIP JOB (640W0090100W980JPIP,W100),'RTN W980D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//MAIL    EXEC WMAILSND,FROM='FROM=NO-REPLY@VOLVOCARS.COM'                      
)SEND                                                                           
 TITLE &MSG                                                                     
//  DD DSN=W.WIMS.CONSTANT(PULSJOUR),DISP=SHR                                   
//  DD *                                                                        
 MAIL                                                                           
 NU PIPER DET PGA ATT &MSG                                                      
)END                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W980JPIP                                         
