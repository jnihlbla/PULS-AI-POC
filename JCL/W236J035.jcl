//W236J035 JOB (640W2360100W236J035,W100),'RTN W236V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W236    EXEC W236P035                                                         
//*                                                                             
//ZIP1    EXEC WZ11TZIP,                                                        
//           DSIN=&&W23636X,                                                    
//           DSOUTZIP=W236.W236V2.W23636X(+1),                                  
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W23636X.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=W236.W236V2.W23636X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=deliveryprecision%YYYYMMDD.zip                              
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W236J035                                         
