//W011J083 JOB (640W0110100W011J083,W100),'RTN W011D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W011    EXEC W011P083                                                         
//*                                                                             
//ZIP1    EXEC WZ11TZIP,                                                        
//           DSIN=&&W01183X,                                                    
//           DSOUTZIP=W011.NDC.W01183X(+1),                                     
//*WDK723 DATA IN READABLE FORMAT                                               
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W01183X.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=W011.NDC.W01183X(+1)                                        
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=ndcprocurement723%YYYYMMDD.zip                              
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W011J083                                         
