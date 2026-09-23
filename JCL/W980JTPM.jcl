//W980JTPM JOB (650W0090100W980JTPM,W100),'RTN W980D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=50,FORMS=1800                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*CNTL  W980JTPM,EXC                                                            
//*                                                                             
//BKUP    EXEC WSOPCOPY,BLOCKS=1000,                                            
//             SOPREG2=W.DUMP.QASE.SOP(+1)                                      
//*                                                                             
//SOP1    EXEC WSOP                                                             
PASSIVATE TIMES-PM                                                              
ACTIVATE  TIMES-PM                                                              
IF-STATUS WBMPSTUP STARTED                                                      
  END     WBMPSTUP                                                              
END-IF                                                                          
IF-STATUS WDAYSTUP STARTED                                                      
  END     WDAYSTUP                                                              
END-IF                                                                          
//*                                                                             
//* -- END PÅ OVANSTÅENDE "STUP"-PROCESSER SOM FINNS FÖR ATT HÅLLA              
//* -- WBMP OCH WDAY STARTADE TILLS DETTA JOB AKTIVERAT KLOCKSLAGEN             
//* -- I TIMES-PM PÅ RADEN INNAN.                                               
//*                                                                             
//* -- EJ SOPEND PGA ATT FÖRÄLDERN TIMES-PM OMAKTIVERAS OVAN                    
//* -- OCH DÄRFÖR ÄR DETTA JOBBET WAITING I DETTA LÄGET                         
//* -- DOCK ABEND OM NÅGOT PROBLEM UPPSTÅTT, SÅ ATT SIGNAL SKICKAS.             
//*                                                                             
//  IF (RC >= 8) THEN                                                           
//VRCABE  EXEC VRCABEND                                                         
//  ENDIF                                                                       
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W980JTPM                                                                  
