//W114S7RS JOB (640W1140100W114S7RS,W100),'RTN W114S7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//RENAME  EXEC W001PTSO                                                         
//SYSTSIN  DD *                                                                 
  %WRTNIN2  W114.W114X7SE.T335R309   W114.W114S7.T335R309                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114S7RS                                         
