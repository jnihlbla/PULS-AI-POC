//W483J027 JOB (640W4830100W483J027,W100),'RTN W488D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W483    EXEC W483P027                                                         
//*                                                                             
//ZIP1     EXEC WZ11TZIP,                                                       
//           DSIN=W483.W488D2.W48327(+1),                                       
//           DSOUTZIP=W483.W488D2.W48327X(+1),                                  
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=WDB501.CSV,ZIPDATAC=PSEB                                   
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=W483.W488D2.W48327X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=DealerFreightCodeV1%YYYYMMDD.zip                            
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W483J027                                         
