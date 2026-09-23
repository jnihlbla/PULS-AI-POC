//W335J0E7 JOB (640W3350100W335J0E7,W100),'RTN W335E7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WQREC   EXEC WZ11P013                                                         
//WZ1113.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.ORDEROFPARTINFOFILE                                    
¤DSOUT W335.W335X7*Market*.W33507                                               
¤RECFM FB                                                                       
¤LRECL 21                                                                       
¤SOPSYM Market;MARKET                                                           
//*                                                                             
//SOPSET1 EXEC WSOP                                                             
//W98022D1 DD  *                                                                
  ORDER W335B2 SYMBOLS                                                          
//   DD DSN=&&SOPSYM,DISP=(OLD,DELETE)                                          
//   DD *                                                                       
  END-ORDER                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J0E7                                         
