//W412X5SE JOB (640W4120100W412X5SE,W100),'RTN W412X5',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W412X5 SYMBOLS                                                          
    VCOM(W412X5SE)                                                              
  END-ORDER                                                                     
//*                                                                             
