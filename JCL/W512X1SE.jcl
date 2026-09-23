//W512X1SE JOB (640W5120100W512X1SE,W100),'RTN W512X1',                         
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W512X1 SYMBOLS                                                          
    VCOM(W512X1SE)                                                              
  END-ORDER                                                                     
//*                                                                             
