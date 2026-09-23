//W418J047 JOB (640W4180100W418J047,W100),'RTN W418D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W418    EXEC W418P047                                                         
//*                                                                             
//ZIP1    EXEC WZ11TZIP,                                                        
//           DSIN=&&W41842X,                                                    
//           DSOUTZIP=W418.W418D6.W41842X(+1),                                  
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W41842X.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W418.W418D6.W41842X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=discrepancies%YYYYMMDD.zip                                  
/*                                                                              
//*DELETE PASS FILE Y2 AFTER FIRST RUN                                          
//COPY29   EXEC PGM=ICEGENER                                                    
//SYSPRINT DD  SYSOUT=*                                                         
//SYSUT1   DD  DSN=&&W41842Y,DISP=(OLD,DELETE,DELETE)                           
//SYSUT2   DD  DUMMY                                                            
//*                                                                             
//W418NE8 EXEC W418P047                                                         
//W41847.W41847D1 DD DSN=&INDIN..W41842X1(+0),DISP=SHR                          
//*FILE FOR DISCREPANCY STATUS <> 8                                             
//ZIP2    EXEC WZ11TZIP,                                                        
//           DSIN=&&W41842Y,                                                    
//           DSOUTZIP=W418.W418D6.W41842Y(+1),                                  
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W41842Y.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W418.W418D6.W41842Y(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=opendiscrepancies%YYYYMMDD.zip                              
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W418J047                                         
