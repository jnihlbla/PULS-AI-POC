//W092X4PP JOB (540W0920100W092X4PP,W100),'RTN W092X4',                         
//             CLASS=K                                                          
/*JOBPARM TIME=1,LINES=5,FORMS=1800,LINECT=0                                    
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W092X4 SYMBOLS                                                          
    VCOM(W092X4PP)                                                              
  END-ORDER                                                                     
//*                                                                             
