//W236J01A JOB (640W2360100W236J01A,W100),'RTN W236D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W236    EXEC W236P01A                                                         
//*                                                                             
//ZIP     EXEC WZ11TZIP,                                                        
//           DSIN=&&W2361AX,                                                    
//           DSOUTZIP=W236.W236D5.W2361AX(+1),                                  
//*WDD902 DATA IN READABLE FORMAT                                               
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W2361AX.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W236.W236D5.W2361AX(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=WDD902Deliveryplan%YYYYMMDD.zip                             
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W236J01A                                         
