//W215J0X1 JOB (640W2150100W215J0X1,W100),'RTN W215X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//*                                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WQREC   EXEC WZ11P012,ABSADDRS=CARPARTS.KDP.KITINFO,                          
//             DSOUT=W215.W215X1SE.W21501,                                      
//             RECFM=FB,LRECL=125                                               
//*                                                                             
//SOP     EXEC WSOP                                                             
    ORDER W215D1                                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W215J0X1                                         
