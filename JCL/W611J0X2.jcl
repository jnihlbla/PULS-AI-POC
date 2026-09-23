//W611J0X2 JOB (640W6110100W611J0X2,W100),'RTN W611X2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//*                                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WQREC   EXEC WZ11P012,ABSADDRS=CARPARTS.KDP.WEIGHTINFO,                       
//             DSOUT=W611.W611X2SE.W61106,                                      
//             RECFM=FB,LRECL=80                                                
//*                                                                             
//SOP     EXEC WSOP                                                             
  ORDER W611S9                                                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J0X2                                         
