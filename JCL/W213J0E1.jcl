//W213J0E1 JOB (640W2130100W213J0E1,W100),'RTN W213E1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WQREC   EXEC WZ11P013                                                         
//*                                                                             
//WZ1113.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTNERMASTER                                          
¤DSOUT W213.W213X1SE.A31481                                                     
¤RECFM FB                                                                       
¤LRECL 426                                                                      
/*                                                                              
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W213D1                                                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W213J0E1                                         
