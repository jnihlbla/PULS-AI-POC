//W261X1P0 JOB (640W2610100W261X1P0,W100),'RTN W261X1',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W261X1 SYMBOLS                                                          
    VCOM(W261X1P0)                                                              
  END-ORDER                                                                     
//*                                                                             
