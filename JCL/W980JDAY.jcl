//W980JDAY JOB (650W0090100W980JDAY,W100),'RTN W980D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=50,FORMS=1800                                                   
/*AFTER W980JLST                                                                
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*CNTL  WSOPDAT,EXC                                                             
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W980JDAY                                             
//*                                                                             
//SOP1    EXEC WSOP                                                             
IF-STATUS WDAY STARTED                                                          
  END WDAY                                                                      
ENDIF                                                                           
ACTIVATE WDAY                                                                   
//*                                                                             
//* -- WDAY LÄGGS INTE I HOLD LÄNGRE!!                                          
//* -- WDAY LÄGGS I HOLD FÖR ATT INGET SKA GÅ IGÅNG INNAN                       
//* -- EFTERMIDDAGENS KLOCKSLAG HUNNITS AKTIVERAS I W980JTPM                    
//*                                                                             
//  IF (RC >= 8) THEN                                                           
//ABEND   EXEC VRCABEND                                                         
//  ENDIF                                                                       
//*                                                                             
//FREE    EXEC WFREE,NAME=W980JDAY                                              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W980JDAY                                         
