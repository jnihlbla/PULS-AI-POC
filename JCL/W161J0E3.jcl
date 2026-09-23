//W161J0E3 JOB (640W1610100W161J0E3,W100),'RTN W161E3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WQREC   EXEC WZ11P013                                                         
//WZ1113.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.NEWBIMAPARTTOPULS                                      
¤DSOUT W161.W161X3SE.W16150                                                     
¤RECFM FB                                                                       
¤LRECL 256                                                                      
//*                                                                             
//SOP    EXEC WSOP                                                              
  ORDER W161S3                                                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W161J0E3                                         
