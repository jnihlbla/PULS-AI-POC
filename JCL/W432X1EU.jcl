//W432X1EU JOB (640W4320100W432X1EU,W100),'RTN W432E1',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W432E1 SYMBOLS                                                          
    VCOM(W432X1EU)                                                              
  END-ORDER                                                                     
//*                                                                             
