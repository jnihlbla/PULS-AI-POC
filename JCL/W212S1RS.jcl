//W212S1RS JOB (640W2120100W212S1RS,W100),'RTN W212S1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W212S1                                               
//*                                                                             
//RENAME  EXEC W001PTSO                                                         
//TSO.SYSTSIN  DD *                                                             
  %WRTNIN2  W092.W092X4PP.EPIC   W092.W212S1.EPIC                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W212S1RS                                         
