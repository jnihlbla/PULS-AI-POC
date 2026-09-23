//W483J026 JOB (640W4830100W483J026,W100),'RTN W483V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W483    EXEC W483P026                                                         
//*                                                                             
//ZIP1    EXEC WZ11TZIP,                                                        
//           DSIN=&&W48325X,                                                    
//           DSOUTZIP=W483.W483V2.W48325X(+1),                                  
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W48325X.CSV                                                
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W483.W483V2.W48325X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=InvoicedCases%YYYYMMDD.ZIP                                  
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W483J026                                         
