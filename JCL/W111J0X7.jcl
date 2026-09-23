//W111J0X7 JOB (640W1110100W111J0X7,W100),'RTN W111X7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WQREC   EXEC WZ11P012,ABSADDRS=CARPARTS.MIC.PCOO,                             
//             DSOUT=W111.W111X7SE.W11150,                                      
//             RECFM=VB,LRECL=10003                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W111D2                                                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W111J0X7                                         
