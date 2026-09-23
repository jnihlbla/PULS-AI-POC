//W517J024 JOB (650W5170100W517J024,W100),'RTN W517V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W517   EXEC W517P024                                                          
//*                                                                             
//ZIP1    EXEC WZ11TZIP,                                                        
//           DSIN=&&W51724X,                                                    
//           DSOUTZIP=W517.W517V1.W51724X(+1),                                  
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W51724X.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W517.W517V1.W51724X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=salesfigures%YYYYMMDD.zip                                   
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W517J024                                         
