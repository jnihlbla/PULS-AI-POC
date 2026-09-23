//W478J016 JOB (650W4780100W478J016,W100),'RTN W478D3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
/*JOBPARM LINES=999                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W478    EXEC W478P016                                                         
//ZIP1    EXEC WZ11TZIP,                                                        
//           DSIN=W478.W478D3.W47816(+1),                                       
//           DSOUTZIP=W478.W478D3.ZIP.W47816X(+1),                              
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W47816X.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=W478.W478D3.ZIP.W47816X(+1)                                 
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=TransporterInfoV1%YYYYMMDD.ZIP                              
/*                                                                              
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W478J016                                         
