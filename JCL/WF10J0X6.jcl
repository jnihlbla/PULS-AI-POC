//WF10J0X6 JOB (640WF100100WF10J0X6,W100),'RTN WF10X6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//*                                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WQREC   EXEC WZ11P012,ABSADDRS=CARPARTS.SAP.MASTERDATA,                       
//             DSOUT=WF10.WF10X6SE.WF1050,                                      
//             RECFM=VB,LRECL=1058                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10J0X6                                         
