//W330X1M2 JOB (640W3300100W330X1M2,W100),'W330X1',                             
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W330X1 SYMBOLS                                                          
    VCOM(W330X1M2)                                                              
  END-ORDER                                                                     
//*                                                                             
