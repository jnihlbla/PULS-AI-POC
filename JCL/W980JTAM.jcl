//W980JTAM JOB (650W0090100W980JTAM,W100),'RTN W980D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=50,FORMS=1800                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*CNTL  W980JTAM,EXC                                                            
//*                                                                             
//BKUP    EXEC WSOPCOPY,BLOCKS=1000,                                            
//             SOPREG2=W.DUMP.QASE.SOP(+1)                                      
//*                                                                             
//SOP1    EXEC WSOP                                                             
PASSIVATE TIMES-AM                                                              
ACTIVATE  TIMES-AM                                                              
//*                                                                             
//* -- EJ SOPEND PGA ATT FÖRÄLDERN TIMES-AM OMAKTIVERAS OVAN                    
//* -- OCH DÄRFÖR ÄR DETTA JOBBET WAITING I DETTA LÄGET                         
//* -- DOCK ABEND OM NÅGOT PROBLEM UPPSTÅTT, SÅ ATT SIGNAL SKICKAS.             
//*                                                                             
//  IF (RC >= 8) THEN                                                           
//VRCABE  EXEC VRCABEND                                                         
//  ENDIF                                                                       
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W980JTAM                                                                  
