//WBATCHS9 JOB (540W0090100WBATCHS9,W100),'RTN W980D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=5,FORMS=1800                                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WBATCHS9                                         
//*                                                                             
//SOPINST EXEC WSOP                                                             
IF-CALENDAR BATCH06                                                             
  ACTIVATE W980JPIP SYMBOLS                                                     
    MSG(NU BÖRJAR WBATCH9)                                                      
  END-ACTIVATE                                                                  
END-IF                                                                          
IF-CALENDAR BATCH01                                                             
  ACTIVATE W980JPIP SYMBOLS                                                     
    MSG(NU BÖRJAR WBATCH9)                                                      
  END-ACTIVATE                                                                  
END-IF                                                                          
IF-CALENDAR PIP                                                                 
  ACTIVATE W980JPIP SYMBOLS                                                     
    MSG(NU BÖRJAR WBATCH9)                                                      
  END-ACTIVATE                                                                  
END-IF                                                                          
