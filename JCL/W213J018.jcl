//W213J018 JOB (640W2130100W213J018,W100),'RTN W213D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W213    EXEC W213P018                                                         
//*                                                                             
//ZIP1    EXEC WZ11TZIP,                                                        
//           DSIN=&&W21320X,                                                    
//           DSOUTZIP=W213.W213D1.W21320X(+1),                                  
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W21320X.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W213.W213D1.W21320X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=suppliers%YYYYMMDD.ZIP                                      
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W213J018                                         
