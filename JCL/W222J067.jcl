//W222J067 JOB (640W2220100W222J067,W100),'RTN W200V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W222    EXEC W222P067                                                         
//*                                                                             
//ZIP     EXEC WZ11TZIP,                                                        
//           DSIN=&&W22267,                                                     
//           DSOUTZIP=W222.W200V1.W22267.ZIP(+1),                               
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=MADLOG.CSV,ZIPDATAC=PSEB                                   
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W222.W200V1.W22267.ZIP(+1)                                  
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=forecastDeviationDC11PeriodEndLog%YYYYMMDD.zip              
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W222J067                                         
