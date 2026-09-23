//W011J082 JOB (640W0110100W011J082,W100),'RTN W011D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W011    EXEC W011P082                                                         
//*                                                                             
//ZIP1    EXEC WZ11TZIP,                                                        
//           DSIN=&&W01182X,                                                    
//           DSOUTZIP=W011.NDC.W01182X(+1),                                     
//*WDK722 DATA IN READABLE FORMAT                                               
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W01182X.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W011.NDC.W01182X(+1)                                        
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=ndcprocurement722%YYYYMMDD.zip                              
/*                                                                              
//ZIP2    EXEC WZ11TZIP,                                                        
//           DSIN=&&W01183X,                                                    
//           DSOUTZIP=W011.NDC.W01183X(+1),                                     
//*WDK723 DATA IN READABLE FORMAT                                               
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W01183X.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN2  EXEC WZ11P023,                                                        
//             DSIN=W011.NDC.W01183X(+1)                                        
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=ndcprocurement723%YYYYMMDD.zip                              
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W011J082                                         
