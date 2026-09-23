//W440J06B JOB (640W4400100W440J06B,W100),'RTN W440D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
//      SET INDUT=W440.W440D3                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W440    EXEC W440P06B                                                         
//*                                                                             
//ZIP1    EXEC WZ11TZIP,                                                        
//           DSIN=&&W4406B1,                                                    
//           DSOUTZIP=&INDUT..W4406B1(+1),                                      
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W4406B1.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=&INDUT..W4406B1(+1)                                         
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=VOROpenorder%YYYYMMDD.ZIP                                   
/*                                                                              
//ZIP2    EXEC WZ11TZIP,                                                        
//           DSIN=&&W4406B2,                                                    
//           DSOUTZIP=&INDUT..W4406B2(+1),                                      
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W4406B2.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=&INDUT..W4406B2(+1)                                         
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=VORhistoryorder%YYYYMMDD.ZIP                                
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W440J06B                                         
