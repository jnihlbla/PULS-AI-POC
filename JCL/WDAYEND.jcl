//WDAYEND  JOB (540W0090100WDAYEND,W100),'RTN W980D1',                          
//             USER=?,PASSWORD=?,                                               
//        CLASS=L                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=5,FORMS=1800,LINECT=0                                           
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*CNTL  WSOPDAT,EXC                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
END WDAYEND                                                                     
IF-STATUS WDAY STARTED                                                          
  END WDAY                                                                      
ENDIF                                                                           
//*                                                                             
// EXEC WWAIT,SECONDS=60   -- GE EV PÅGÅENDE RUTIN CHANSEN ATT GÅ KLAR          
