//W116X2PA JOB (640W1160100W116X2PA,W100),'RTN W116E2',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W116E2 SYMBOLS                                                          
    VCOM(W116X2PA)                                                              
  END-ORDER                                                                     
//*                                                                             
