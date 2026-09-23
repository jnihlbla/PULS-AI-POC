//W111X7SE JOB (640W1110100W111X7SE,W100),'RTN W111X7',                         
//             CLASS=K,USER=W0MQT01                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W111X7                                                                  
//*                                                                             
