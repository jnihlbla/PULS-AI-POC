//W114X4SE JOB (640W1140100W114X4SE,W100),'RTN W114X4',                         
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
  ORDER W114X4 SYMBOLS                                                          
    VCOM(W114X4SE)                                                              
  END-ORDER                                                                     
//*                                                                             
//*SOPEND  EXEC WSOPEND,PROCESS=W114X4SE                                        
