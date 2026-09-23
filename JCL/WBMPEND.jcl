//WBMPEND  JOB (540W0090100WBMPEND,W100),'RTN W980D1',                          
//        CLASS=L                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM TIME=1,LINES=50,CARDS=0,FORMS=1800                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*CNTL  WSOPDAT,EXC                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
END WBMPEND                                                                     
IF-STATUS WBMP STARTED                                                          
  END WBMP                                                                      
ENDIF                                                                           
//*                                                                             
// EXEC WWAIT,SECONDS=60   -- GE EV PÅGÅENDE RUTIN CHANSEN ATT GÅ KLAR          
