//W418J0E3 JOB (670WZ110100W418J0E3,W100),'RTN W418E3',                         
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
¤ADDISPABS CARPARTS.PULS.DEALERRETURNQTYCONFIRM                                 
¤DSOUT W418.W418X2MQ.W41802                                                     
¤RECFM VB                                                                       
¤LRECL 33                                                                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W418J0E3                                         
