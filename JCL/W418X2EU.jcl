//W418X2EU JOB (640W4180100W418X2EU,W100),'RTN W418E2',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W418E2 SYMBOLS                                                          
    VCOM(W418X2EU)                                                              
  END-ORDER                                                                     
//*                                                                             
