//W116X1BE JOB (640W1160100W116X1BE,W100),'RTN W116X1',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W116X1 SYMBOLS                                                          
    VCOM(W116X1BE)                                                              
  END-ORDER                                                                     
//*                                                                             
