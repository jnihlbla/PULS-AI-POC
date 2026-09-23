//W330J0E1 JOB (640W3300100W330J0E1,W100),'RTN W330E1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WQREC   EXEC WZ11P013                                                         
//WZ1113.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PRICEAREASALESSTATISTIC                                
¤DSOUT W330.W330X1*Market*.W33033                                               
¤RECFM VB                                                                       
¤LRECL 84                                                                       
¤SOPSYM Market;MARKET                                                           
//*                                                                             
//ORDER   EXEC WSOP                                                             
//W98022D1 DD  *                                                                
  ACTIVATE W330J034 SYMBOLS                                                     
//   DD DSN=&&SOPSYM,DISP=(OLD,DELETE)                                          
//   DD *                                                                       
  END-ACTIVATE                                                                  
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W330J0E1                                         
