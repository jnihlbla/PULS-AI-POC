//W152J0X1 JOB (640W1520100W152J0X1,W100),'RTN W152X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WMQREC  EXEC WZ11P012,ABSADDRS=CARPARTS.CBG.TRANSLATEDDESC,                   
//             DSOUT=W152.W152X1.W15205,                                        
//* IF QMGR NOT GIVEN, DEFAULT WILL BE NULL AND PROGRAM SETS IT.                
//             RECFM=VB,LRECL=500                                               
//* IF RECFM, LRECL NOT GIVEN, DEFAULT WILL BE VB AND 3000                      
//*                                                                             
//*                                                                             
//ORDER  EXEC WSOP                                                              
    ORDER W152B1                                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W152J0X1                                         
