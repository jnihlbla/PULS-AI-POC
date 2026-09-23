//W981JTFM JOB (650W0090100W981JTFM,W100),'RTN W981D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*JOBPARM LINES=50,FORMS=1800                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*CNTL  W980JTAM,EXC                                                            
//*                                                                             
//BKUP    EXEC WSOPCOPY,BLOCKS=1000,                                            
//             SOPREG2=W.DUMP.PROD.SOP(+1)                                      
//*                                                                             
//SOP1    EXEC WSOP                                                             
PASSIVATE TIDER-FM                                                              
ACTIVATE  TIDER-FM                                                              
//*                                                                             
//* -- EJ SOPEND PGA ATT FÖRÄLDERN TIMES-FM OMAKTIVERAS OVAN                    
//* -- OCH DÄRFÖR ÄR DETTA JOBBET WAITING I DETTA LÄGET                         
//* -- DOCK ABEND OM NÅGOT PROBLEM UPPSTÅTT, SÅ ATT SIGNAL SKICKAS.             
//*                                                                             
//  IF (RC >= 8) THEN                                                           
//VRCABE  EXEC VRCABEND                                                         
//  ENDIF                                                                       
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W981JTFM                                                                  
