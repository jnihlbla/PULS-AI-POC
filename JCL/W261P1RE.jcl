//W261P1RE JOB (650W2610100W261P1RE,W100),'RTN W261P1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800                                                            
/*ROUTE  XEQ  LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//ORDER EXEC WSOP                                                               
IF-CALENDAR FEB                                                                 
  ACTIVATE W261B1                                                               
ENDIF                                                                           
IF-CALENDAR MAR                                                                 
  ACTIVATE W261B1                                                               
ENDIF                                                                           
IF-CALENDAR SEP                                                                 
  ACTIVATE W261B1                                                               
ENDIF                                                                           
//SOP     EXEC WSOPEND,PROCESS=W261P1RE                                         
/*                                                                              
