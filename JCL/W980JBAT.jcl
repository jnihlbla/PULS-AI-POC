//W980JBAT   JOB (540W0090100W980JBAT,W100),'RTN W980D1',                       
//           USER=?,PASSWORD=?,                                                 
//           CLASS=L                                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=50,FORMS=1800                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W980JBAT                                             
//*                                                                             
//ACTIVATE EXEC WSOP                                                            
//SYSUDUMP DD SYSOUT=(D,,DUMP)                                                  
  ACTIVATE WBATCH                                                               
  ORDER    WBATCH2                                                              
  IF-CALENDAR DAY                                                               
    IF-CALENDAR NOT-BATCH01-1                                                   
      ORDER    WBATCH2                                                          
    ENDIF                                                                       
  ENDIF                                                                         
  IF-CALENDAR WEEK                                                              
    ACTIVATE WBATCHPA                                                           
  ENDIF                                                                         
  ACTIVATE WBATCHNA                                                             
  ACTIVATE WBATCH9                                                              
  IF-CALENDAR BATCH01                                                           
    START WBATCHS1                                                              
    START WBATCHS2                                                              
    START WBATCHS3                                                              
    START WBATCHS4                                                              
    ACTIVATE W980JPIP SYMBOLS                                                   
      MSG(VECKOBATCH STARTAR)                                                   
    END-ACTIVATE                                                                
  ENDIF                                                                         
  IF-CALENDAR BATCH06                                                           
    IF-STATUS WBMPEND WAITING                                                   
      START WBMPEND                                                             
    ENDIF                                                                       
    START WBATCHS1                                                              
    START WBATCHS2                                                              
    START WBATCHS3                                                              
    START WBATCHS4                                                              
    ACTIVATE W980JPIP SYMBOLS                                                   
      MSG(VECKOBATCH STARTAR)                                                   
    END-ACTIVATE                                                                
  ENDIF                                                                         
//*                                                                             
//  IF (RC >= 8) THEN                                                           
//ABEND   EXEC VRCABEND                                                         
//  ENDIF                                                                       
//*                                                                             
//FREE    EXEC WFREE,NAME=W980JBAT                                              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W980JBAT                                         
