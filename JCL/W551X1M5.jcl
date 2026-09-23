//W551X1M5 JOB (640W5510100W551X1M5,W100),'RTN W551X1',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W551X1 SYMBOLS                                                          
    VCOM(W551X1M5) MCOMP(M5)                                                    
  END-ORDER                                                                     
//*                                                                             
