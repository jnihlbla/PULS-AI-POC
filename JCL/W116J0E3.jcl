//W116J0E3 JOB (640W1160100W116J0E3,W100),'RTN W116E3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WQREC   EXEC WZ11P013                                                         
//WZ1113.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTSUPERSESSIONREQUEST                                
¤DSOUT W116.W116E3.W11601                                                       
¤RECFM FB                                                                       
¤LRECL 80                                                                       
¤SOPSYM Market;COUNTRYX2                                                        
//*                                                                             
//ORDER   EXEC WSOP                                                             
//W98022D1 DD  *                                                                
  ORDER W116J050 SYMBOLS                                                        
//   DD DSN=&&SOPSYM,DISP=(OLD,DELETE)                                          
//   DD *                                                                       
  END-ORDER                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W116J0E3                                         
