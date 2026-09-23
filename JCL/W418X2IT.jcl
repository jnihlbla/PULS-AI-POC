//W418X2IT JOB (640W4180100W418X2IT,W100),'RTN W418X2',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W418X2 SYMBOLS                                                          
    VCOM(W418X2IT)                                                              
  END-ORDER                                                                     
//*                                                                             
