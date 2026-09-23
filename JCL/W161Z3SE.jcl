//W161Z3SE JOB (640W1610100W161Z3SE,W100),'RTN W161S3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W161.W161S3.W16151(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.NEWPARTRESPONSE                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W161Z3SE                                         
