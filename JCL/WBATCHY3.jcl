//WBATCHY3 JOB (540W0090100WBATCHY3,W100),'RTN W980D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=5,FORMS=1800                                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
 IF-CALENDAR DAY                                                                
 IF-CALENDAR NOT-WEEK                                                           
   ACTIVATE WBATCHQ3                                                            
 END-IF                                                                         
 END-IF                                                                         
 IF-CALENDAR WEEK                                                               
   IF-CALENDAR NOT-BATCH01                                                      
   IF-CALENDAR NOT-BATCH06                                                      
     ACTIVATE WBATCHQ3                                                          
   END-IF                                                                       
   END-IF                                                                       
 END-IF                                                                         
 IF-CALENDAR BATCH06                                                            
   IF-STATUS TIME0500 WAITING                                                   
     ACTIVATE WBATCHQ3                                                          
   END-IF                                                                       
 END-IF                                                                         
//*                                                                             
//  IF (RC >= 8) THEN                                                           
//ABEND   EXEC VRCABEND                                                         
//  ENDIF                                                                       
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WBATCHY3                                         
