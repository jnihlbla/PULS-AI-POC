//W980JBMP JOB (650W0090100W980JBMP,W100),'RTN W980D1',                         
//        USER=?,PASSWORD=?,                                                    
//        CLASS=L                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=50,FORMS=1800                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*CNTL  WSOPDAT,EXC                                                             
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W980JBMP                                             
//*                                                                             
//SOP1    EXEC WSOP                                                             
IF-CALENDAR IMS                                                                 
 IF-STATUS WBMP STARTED                                                         
   END WBMP                                                                     
 END-IF                                                                         
 ACTIVATE WBMP                                                                  
END-IF                                                                          
//*                                                                             
//* -- WBMP LÄGGS INTE I HOLD LÄNGRE!!                                          
//* -- WBMP LÄGGS I HOLD FÖR ATT INGET SKA GÅ IGÅNG INNAN                       
//* -- EFTERMIDDAGENS KLOCKSLAG HUNNITS AKTIVERAS I W980JTPM                    
//*                                                                             
//  IF (RC >= 8) THEN                                                           
//ABEND   EXEC VRCABEND                                                         
//  ENDIF                                                                       
//*                                                                             
//FREE    EXEC WFREE,NAME=W980JBMP                                              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W980JBMP                                         
