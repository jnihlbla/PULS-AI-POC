//W011J08A JOB (640W0110100W011J08A,W100),'RTN W011D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W011    EXEC W011P08A                                                         
//*                                                                             
//ZIP     EXEC WZ11TZIP,                                                        
//           DSIN=&&W01182EX,                                                   
//           DSOUTZIP=W011.NDC.W01182EX.ZIP(+1),                                
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W01182EX.CSV,ZIPDATAC=PSEB                                 
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W011.NDC.W01182EX.ZIP(+1)                                   
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=ndcprocurement722V2%YYYYMMDD.zip                            
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011J08A                                         
