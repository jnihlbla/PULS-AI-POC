//W114J0E3 JOB (640W1140100W114J0E3,W100),'RTN W114E3',                         
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
¤ADDISPABS CARPARTS.PULS.CERTTOPULS                                             
¤DSOUT W114.W114X3SE.W11410                                                     
¤RECFM FB                                                                       
¤LRECL 300                                                                      
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W114J0E3                                         
