//W418X2PL JOB (640W4180100W418X2PL,W100),'RTN W418X2',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W418X2 SYMBOLS                                                          
    VCOM(W418X2PL)                                                              
  END-ORDER                                                                     
//*                                                                             
