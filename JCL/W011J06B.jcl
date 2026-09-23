//W011J06B JOB (640W0110100W011J06B,W100),'RTN W011D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W011    EXEC W011P06B                                                         
//*                                                                             
//ZIP     EXEC WZ11TZIP,                                                        
//           DSIN=W011.W011D6.W01160EY(+1),                                     
//           DSOUTZIP=W011.W011D6.W01160EY.ZIP(+1),                             
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W01160EY.CSV,ZIPDATAC=PSEB                                 
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W011.W011D6.W01160EY.ZIP(+1)                                
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=cdcmasterV4%YYYYMMDD.ZIP                                    
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011J06B                                         
