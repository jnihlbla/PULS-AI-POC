//WXTRJ0A7 JOB (640WXTR0100WXTRJ0A7,W100),'RTN WXTRD1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTÖ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WXTR    EXEC WXTRP0A7                                                         
//*                                                                             
//ZIP     EXEC WZ11TZIP,                                                        
//           DSIN=&&WXTRA7X,                                                    
//           DSOUTZIP=WXTR.WXTRD1.WXTRA7X(+1),                                  
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=WXTRA7X.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=WXTR.WXTRD1.WXTRA7X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=packedlines%YYYYMMDD.ZIP                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ0A7                                         
