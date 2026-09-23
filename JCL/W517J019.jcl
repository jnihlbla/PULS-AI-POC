//W517J019 JOB (640W5170100W517J019,W100),'RTN W517V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W517    EXEC W517P019                                                         
//*                                                                             
//ZIP1    EXEC WZ11TZIP,                                                        
//           DSIN=&&W51716X,                                                    
//           DSOUTZIP=W517.W517V1.W51716X(+1),                                  
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W51716X.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W517.W517V1.W51716X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=stockvalue%YYYYMMDD.zip                                     
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W517J019                                         
