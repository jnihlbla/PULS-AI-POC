//WF10J0X5 JOB (640WF100100WF10J0X5,W100),'RTN WF10X5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//*                                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WQREC   EXEC WZ11P012,ABSADDRS=CARPARTS.SAP.EXCHANGERATE,                     
//             DSOUT=WF10.WF10X5SE.WF1040,                                      
//             RECFM=FB,LRECL=44                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10J0X5                                         
