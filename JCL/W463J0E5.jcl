//W463J0E5 JOB (640W4630100W463J0E5,W100),'RTN W463E5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WQREC   EXEC WZ11P013                                                         
//*                                                                             
//WZ1113.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.SALESINFO                                              
¤DSOUT W463.W463X5DE.W46360                                                     
¤RECFM FB                                                                       
¤LRECL 160                                                                      
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W463J0E5                                         
