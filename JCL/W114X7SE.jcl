//W114X7SE JOB (640W1140100W114X7SE,W100),'RTN W114X7',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*  DENNA JCL ÄR STARTAD AV VCOM                                               
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W114X7 SYMBOLS                                                          
    VCOM(W114X7SE)                                                              
  END-ORDER                                                                     
//*                                                                             
//*SOPEND  EXEC WSOPEND,PROCESS=W114X7SE                                        
