//W233J0X1 JOB (640W2330100W233J0X1,W100),'RTN W233X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//*                                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WQREC   EXEC WZ11P012,ABSADDRS=CARPARTS.HERCULES.GETSPAREPART,                
//             DSOUT=W233.W233X1SE.W23327,                                      
//             RECFM=FB,LRECL=32                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W233J0X1                                         
