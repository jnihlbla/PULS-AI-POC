//W980J071 JOB (640W0090100W980J071,W100),'RTN W980P1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
//     INCLUDE MEMBER=DESTN                                                     
/*JOBPARM FORMS=1800,LINECT=0,LINES=999                                         
/*ROUTE   XEQ NJEVC                                                             
/*ROUTE PRINT NJOVC                                                             
//ISPF    EXEC W001ISPF                                                         
//SYSTSIN  DD *                                                                 
ISPSTART CMD(%W980MAIL W980.W980P1 )                                            
//*                                                                             
//W980    EXEC W980P071,INDGRND=W.PROD                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W980J071                                         
