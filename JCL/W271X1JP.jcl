//W271X1JP JOB (640W2710100W271X1JP,W100),'RTN W271X1',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//SOP     EXEC WSOP                                                             
  ORDER W271X1 SYMBOLS                                                          
    VCOM(W271X1JP)                                                              
  END-ORDER                                                                     
//*                                                                             
