//WBATCHZ2 JOB (540W0090100WBATCHZ2,W100),'RTN W980D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=5,FORMS=1800                                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
 IF-CALENDAR AFTERWEEK                                                          
   START WBATCHQ2                                                               
 END-IF                                                                         
 IF-CALENDAR BATCH01                                                            
   START WBATCHQ2                                                               
 END-IF                                                                         
//*                                                                             
//  IF (RC >= 8) THEN                                                           
//ABEND   EXEC VRCABEND                                                         
//  ENDIF                                                                       
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WBATCHZ2                                         
