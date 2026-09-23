//W236J01F JOB (640W2360100W236J01F,W100),'RTN W236D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W236    EXEC W236P01F                                                         
//*                                                                             
//ZIP     EXEC WZ11TZIP,                                                        
//           DSIN=&&W2361FX,                                                    
//           DSOUTZIP=W236.W236D5.W2361FX(+1),                                  
//*WDD924 DATA IN READABLE FORMAT                                               
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W2361FX.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W236.W236D5.W2361FX(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=WDD924Deliveryinfo%YYYYMMDD.zip                             
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W236J01F                                         
