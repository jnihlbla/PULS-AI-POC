//W418J0E4 JOB (640W4180100W418J0E4,W100),'RTN W418E4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WQREC    EXEC WZ11P013                                                        
//*                                                                             
//WZ1113.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.DISCREPANCYREQUEST                                     
¤DSOUT W418.W418X1MQ.W41809                                                     
¤RECFM VB                                                                       
¤LRECL 351                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W418J0E4                                         
