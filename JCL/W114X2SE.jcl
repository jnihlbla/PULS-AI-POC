//W114X2SE JOB (540W1140100W114X2SE,W100),'RTN W114X2',                         
//             CLASS=K                                                          
/*JOBPARM TIME=1,LINES=5,FORMS=1800,LINECT=0                                    
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W114X2 SYMBOLS                                                          
    VCOM(W114X2SE)                                                              
  END-ORDER                                                                     
//*                                                                             
