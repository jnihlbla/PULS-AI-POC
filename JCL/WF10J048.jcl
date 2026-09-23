//WF10J048 JOB (640WF100100WF10J048,W100),'RTN WF10D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTF                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//WF10    EXEC WF10P048                                                         
//*                                                                             
//ZIP1    EXEC WZ11TZIP,                                                        
//           DSIN=WF10.WF10D5.DLINARC(+1),                                      
//           DSOUTZIP=WF10.WF10D5.DLINARCX(+1),                                 
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=DLINARCX.CSV,ZIPDATAC=PSEN                                 
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=WF10.WF10D5.DLINARCX(+1)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=arciveinvoicelinesV2%YYYYMMDD.ZIP                           
/*                                                                              
//*                                                                             
//ZIP2    EXEC WZ11TZIP,                                                        
//           DSIN=WF10.WF10D5.DHEAARC(+1),                                      
//           DSOUTZIP=WF10.WF10D5.DHEAARCX(+1),                                 
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=DHEAARCX.CSV,ZIPDATAC=PSEN                                 
//*                                                                             
//WQSEN2  EXEC WZ11P023,                                                        
//             DSIN=WF10.WF10D5.DHEAARCX(+1)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=arciveinvoiceheadersV2%YYYYMMDD.ZIP                         
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=WF10J048                                         
