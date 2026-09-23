//W483J121 JOB (640W4830100W483J021,W100),'RTN W488D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W483    EXEC W483P021,INDUT=&W483..W488D2                                     
//W48321.W48321D1 DD DUMMY                                                      
//*                                                                             
//ZIP1     EXEC WZ11TZIP,                                                       
//           DSIN=W483.W488D2.W48322(+1),                                       
//           DSOUTZIP=W483.W488D2.W48322X(+1),                                  
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=WDE621.CSV,ZIPDATAC=PSEB                                   
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=W483.W488D2.W48322X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=crossdocklinesST9V1%YYYYMMDD.zip                            
/*                                                                              
//*                                                                             
//ZIP2     EXEC WZ11TZIP,                                                       
//           DSIN=W483.W488D2.W48323(+1),                                       
//           DSOUTZIP=W483.W488D2.W48323X(+1),                                  
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=WDE621X.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=W483.W488D2.W48323X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=crossdocklinesST18V1%YYYYMMDD.zip                           
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W483J121                                         
