//WXTRJ0A3 JOB (640W0001000WXTRJ0A3,W100),'RTN WXTRD1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=00                                                  
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//WXTR    EXEC WXTRP0A3                                                         
//*                                                                             
//TZIP    EXEC WZ11TZIP,                                                        
//            DSIN=WXTR.WXTRD1.WXTRA3(+1),                                      
//            DSOUTZIP=WXTR.WXTRD1.ZIP.WXTRA3(+1),                              
//            ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                     
//            CONTENT=WXTRA3.CSV                                                
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=WXTR.WXTRD1.ZIP.WXTRA3(+1)                                  
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=openorders%YYYYMMDD.zip                                     
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WXTRJ0A3                                         
