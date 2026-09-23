//W981JTEM JOB (650W0090100W981JTEM,W100),'RTN W981D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*JOBPARM LINES=50,FORMS=1800                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*CNTL  W980JTPM,EXC                                                            
//*                                                                             
//BKUP    EXEC WSOPCOPY,BLOCKS=1000,                                            
//             SOPREG2=W.DUMP.PROD.SOP(+1)                                      
//*                                                                             
//SOP1    EXEC WSOP                                                             
PASSIVATE TIDER-EM                                                              
ACTIVATE  TIDER-EM                                                              
PASSIVATE WDAG0100                                                              
//*                                                                             
//  IF (RC >= 8) THEN                                                           
//VRCABE  EXEC VRCABEND                                                         
//  ENDIF                                                                       
//SOP     EXEC WSOP,COND=ONLY                                                   
ABEND W981JTEM                                                                  
