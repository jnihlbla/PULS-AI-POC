//W011J08B JOB (640W0110100W011J08B,W100),'RTN W011D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W011    EXEC W011P08B                                                         
//*                                                                             
//ZIP1   EXEC WZ11TZIP,                                                         
//           DSIN=&&W01184L,                                                    
//           DSOUTZIP=W011.LDC.W01184L.ZIP(+1),                                 
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W01184EX.CSV,ZIPDATAC=PSEB                                 
//*                                                                             
//ZIP2   EXEC WZ11TZIP,                                                         
//           DSIN=&&W01184S,                                                    
//           DSOUTZIP=W011.SDC.W01184S.ZIP(+1),ZIPMGMTC=DEL2BKPC,               
//           ZIPDISP=(NEW,CATLG,DELETE),                                        
//           CONTENT=W01184EX.CSV,ZIPDATAC=PSEB                                 
//*                                                                             
//ZIP3   EXEC WZ11TZIP,                                                         
//           DSIN=&&W01184N,                                                    
//           DSOUTZIP=W011.NDC.W01184N.ZIP(+1),ZIPMGMTC=DEL2BKPC,               
//           ZIPDISP=(NEW,CATLG,DELETE),                                        
//           CONTENT=W01184EX.CSV,ZIPDATAC=PSEB                                 
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=W011.LDC.W01184L.ZIP(+1)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=ldcmasterV3%YYYYMMDD.ZIP                                    
/*                                                                              
//WQSEN2  EXEC WZ11P023,                                                        
//             DSIN=W011.SDC.W01184S.ZIP(+1)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=sdcmasterV3%YYYYMMDD.ZIP                                    
/*                                                                              
//WQSEN3  EXEC WZ11P023,                                                        
//             DSIN=W011.NDC.W01184N.ZIP(+1)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=ndcmasterV3%YYYYMMDD.ZIP                                    
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011J08B                                         
