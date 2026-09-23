//W271J324 JOB (640W2710100W271J324,W100),'RTN W271DA',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P024,                                                        
//             INDIN1=W271.W271DA                                               
//*                                                                             
//ZIP     EXEC WZ11TZIP,                                                        
//           DSIN=&&W27120X,                                                    
//           DSOUTZIP=W271.QASE.W27120X(+1),                                    
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W27120X.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W271.QASE.W27120X(+1)                                       
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=refilltable%YYYYMMDD.zip                                    
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J324                                         
