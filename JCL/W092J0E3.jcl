//W092J0E3 JOB (640W0920100W092J0E3,W100),'RTN W092E3',                         
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
¤ADDISPABS CARPARTS.PULS.DIFFPARTFORPULS                                        
¤DSOUT W092.W092X3SE.TIURPROD                                                   
¤RECFM FB                                                                       
¤LRECL 47                                                                       
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W092J0E3                                         
