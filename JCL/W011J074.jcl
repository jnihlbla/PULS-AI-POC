//W011J074 JOB (640W0110100W011J074,W100),'RTN W011D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W011    EXEC W011P074,                                                        
//             INDUT=W011.QASE                                                  
//*                                                                             
//ZIP1    EXEC WZ11TZIP,                                                        
//           DSIN=&&W01174X1,                                                   
//           DSOUTZIP=W011.W011D1.W01174X1(+1),                                 
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W01174X1.CSV,ZIPDATAC=PSEB                                 
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=W011.W011D1.W01174X1(+1)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=partsdescriptions1%YYYYMMDD.ZIP                             
/*                                                                              
//*                                                                             
//ZIP2    EXEC WZ11TZIP,                                                        
//           DSIN=&&W01174X2,                                                   
//           DSOUTZIP=W011.W011D1.W01174X2(+1),                                 
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W01174X2.CSV,ZIPDATAC=PSEB                                 
//*                                                                             
//WQSEN2  EXEC WZ11P023,                                                        
//             DSIN=W011.W011D1.W01174X2(+1)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=partsdescriptions2%YYYYMMDD.ZIP                             
/*                                                                              
//ZIP3    EXEC WZ11BZIP,                                                        
//           DSIN=&&W01174X3,                                                   
//           DSOUTZIP=W011.W011D1.W01174X3(+1),                                 
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W01174X3.CSV,ZIPDATAC=PSEB                                 
//*                                                                             
//WQSEN3  EXEC WZ11P023,                                                        
//             DSIN=W011.W011D1.W01174X3(+1)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=partsdescriptions3%YYYYMMDD.ZIP                             
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W011J074                                         
