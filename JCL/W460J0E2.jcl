//W460J0E2 JOB (670W4600100W460J0E2,W100),'RTN W460E2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WQREC   EXEC WZ11P013                                                         
//WZ1113.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.ORDERREQUEST                                           
¤DSOUT W460.W460X1*Market*.W46001                                               
¤RECFM VB                                                                       
¤LRECL 254                                                                      
¤SOPSYM Market;COUNTRYX2                                                        
//*                                                                             
//ORDER   EXEC WSOP                                                             
//W98022D1 DD  *                                                                
  ORDER W460S3 SYMBOLS                                                          
//   DD DSN=&&SOPSYM,DISP=(OLD,DELETE)                                          
//   DD *                                                                       
  END-ORDER                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W460J0E2                                         
