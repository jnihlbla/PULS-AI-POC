//WDAWEND JOB (540W0090100WDAWEND,W100),'RTN W980D1',                           
//        CLASS=L                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM TIME=1,LINES=5,CARDS=0,FORMS=1800                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
END WDAWEND                                                                     
IF-CALENDAR BATCH01                                                             
  START W980JWEE                                                                
END-IF                                                                          
