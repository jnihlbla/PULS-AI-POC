//W980JBAP   JOB (540W0090100W980JBAP,W100),'RTN W980D1',                       
//           USER=?,PASSWORD=?,                                                 
//           CLASS=L                                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=50,FORMS=1800                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W980JBAP                                             
//*                                                                             
//ACTIVATE EXEC WSOP                                                            
//SYSUDUMP DD SYSOUT=(D,,DUMP)                                                  
  ORDER WBATCH2                                                                 
  ORDER WBATCHPA                                                                
  START WBATCHZ1                                                                
//*                                                                             
//  IF (RC >= 8) THEN                                                           
//ABEND   EXEC VRCABEND                                                         
//  ENDIF                                                                       
//*                                                                             
//FREE    EXEC WFREE,NAME=W980JBAP                                              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W980JBAP                                         
