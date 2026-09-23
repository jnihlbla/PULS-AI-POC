//W418X1RU JOB (640W4180100W418X1RU,W100),'RTN W418X1',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W418X1 SYMBOLS                                                          
    VCOM(W418X1RU)                                                              
  END-ORDER                                                                     
//*                                                                             
