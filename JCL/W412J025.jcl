//W412J025 JOB (640W4120100W412J025,W100),'RTN W412V4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W412    EXEC W412P025                                                         
//*                                                                             
//ZIP1   EXEC WZ11TZIP,                                                         
//           DSIN=&&W41225X,                                                    
//           DSOUTZIP=W412.W412V4.W41225X(+1),ZIPMGMTC=DEL2BKPC,                
//           ZIPDISP=(NEW,CATLG,DELETE),                                        
//           CONTENT=NDC.CSV,ZIPDATAC=PSEN                                      
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=W412.W412V4.W41225X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=DCinformation%YYYYMMDD.ZIP                                  
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W412J025                                         
