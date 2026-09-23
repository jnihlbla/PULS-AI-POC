//W440J060 JOB (650W4400100W440J060,W100),'RTN W440D3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W440    EXEC W440P060                                                         
//*                                                                             
//*ZIP STEP FOR W44094(YESTERDAY FILE WITH BACK ORDER STATUS 4)*                
//TZIP94  EXEC WZ11TZIP,                                                        
//            DSIN=W440.W440D3.W44094(+1),                                      
//            DSOUTZIP=W440.W440D3.ZIP.W44094(+1),                              
//            ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                     
//            CONTENT=W44094.CSV                                                
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=W440.W440D3.ZIP.W44094(+1)                                  
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=backorderst4%YYYYMMDD.zip                                   
/*                                                                              
//*ZIP STEP FOR W44095(FILES WITH BACK ORDER STATUS 1,2,3)*                     
//TZIP95  EXEC WZ11TZIP,                                                        
//            DSIN=W440.W440D3.W44095(+1),                                      
//            DSOUTZIP=W440.W440D3.ZIP.W44095(+1),                              
//            ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                     
//            CONTENT=W44095.CSV                                                
//*                                                                             
//WQSEN2  EXEC WZ11P023,                                                        
//             DSIN=W440.W440D3.ZIP.W44095(+1)                                  
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=backorderst13%YYYYMMDD.zip                                  
/*                                                                              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W440J060                                         
