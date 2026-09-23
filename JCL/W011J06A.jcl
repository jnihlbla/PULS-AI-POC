//W011J06A JOB (640W0110100W011J06A,W100),'RTN W011D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W011    EXEC W011P06A                                                         
//*                                                                             
//ZIP     EXEC WZ11TZIP,                                                        
//           DSIN=&&W01160EX,                                                   
//           DSOUTZIP=W011.W011D6.W01160EX.ZIP(+1),                             
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W01160EX.CSV,ZIPDATAC=PSEB                                 
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W011.W011D6.W01160EX.ZIP(+1)                                
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=cdcmasterV3%YYYYMMDD.ZIP                                    
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011J06A                                         
