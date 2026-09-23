//W371J01B JOB (650W3710100W371J020,W100),'RTN W371V5',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
/*JOBPARM LINES=999                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W371    EXEC W371P01B,                                                        
//        INDIN=&W371..W371V5                                                   
//*                                                                             
//ZIP1    EXEC WZ11TZIP,                                                        
//           DSIN=&&W3711BX,                                                    
//           DSOUTZIP=W371.W371V5.ZIP.W37112X(+1),                              
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=WDM611.CSV,ZIPDATAC=PSEB                                   
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=W371.W371V5.ZIP.W37112X(+1)                                 
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=ExchangeWDM6V1%YYYYMMDD.ZIP                                 
/*                                                                              
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W371J01B                                         
