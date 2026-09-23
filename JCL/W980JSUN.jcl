//W980JSUN   JOB (540W0090100W980JSUN,W100),'RTN W980D1',                       
//           USER=?,PASSWORD=?,                                                 
//           CLASS=L                                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=50,FORMS=1800                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W980JSUN                                             
//*                                                                             
//ACTIVATE EXEC WSOP                                                            
//SYSUDUMP DD SYSOUT=(D,,DUMP)                                                  
  ACTIVATE WBMP                                                                 
  IF-STATUS TIME0400 WAITING                                                    
    START WBMPS004                                                              
  END-IF                                                                        
  IF-STATUS TIME0500 WAITING                                                    
    START WBMPS005                                                              
  END-IF                                                                        
  IF-STATUS TIME0600 WAITING                                                    
    START WBMPS006                                                              
  END-IF                                                                        
  IF-STATUS WBMPSTUP STARTED                                                    
    END   WBMPSTUP                                                              
  END-IF                                                                        
//*                                                                             
//  IF (RC >= 8) THEN                                                           
//ABEND   EXEC VRCABEND                                                         
//  ENDIF                                                                       
//*                                                                             
//FREE    EXEC WFREE,NAME=W980JSUN                                              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W980JSUN                                         
