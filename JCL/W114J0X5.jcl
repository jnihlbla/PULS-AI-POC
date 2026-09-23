//W114J0X5 JOB (640W1140100W114J0X5,W100),'RTN W114X5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//*                                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WQREC   EXEC WZ11P012,ABSADDRS=CARPARTS.KDP.PARTINFO,                         
//             DSOUT=W114.W114X5SE.W11401,                                      
//             RECFM=FB,LRECL=150                                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114J0X5                                         
