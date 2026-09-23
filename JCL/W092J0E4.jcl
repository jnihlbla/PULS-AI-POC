//W092J0E4 JOB (640W0920100W092J0E4,W100),'RTN W092E4',                         
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
¤ADDISPABS CARPARTS.PULS.ORDERRESPONSE                                          
¤DSOUT W092.W092X4PP.EPIC                                                       
¤RECFM VB                                                                       
¤LRECL 86                                                                       
/*                                                                              
//*                                                                             
//SOP    EXEC WSOP                                                              
  ORDER W212S1                                                                  
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W092J0E4                                         
