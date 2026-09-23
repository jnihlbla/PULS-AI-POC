//W981JDAG JOB (650W0090100W981JDAG,W100),'RTN W981D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*JOBPARM LINES=50,FORMS=1800                                                   
/*AFTER W981JLST                                                                
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
/*CNTL  WSOPDAT,EXC                                                             
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W981JDAG                                             
//*                                                                             
//SOP1    EXEC WSOP                                                             
IF-STATUS WDAG STARTED                                                          
  END WDAG                                                                      
ENDIF                                                                           
ACTIVATE WDAG                                                                   
//*                                                                             
//  IF (RC >= 8) THEN                                                           
//ABEND   EXEC VRCABEND                                                         
//  ENDIF                                                                       
//*                                                                             
//FREE    EXEC WFREE,NAME=W981JDAG                                              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W981JDAG                                         
