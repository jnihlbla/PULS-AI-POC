//W112J024 JOB (640W1120100W112J024,W100),'RTN W112D1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W112    EXEC W112P024                                                         
//*                                                                             
//TZIP    EXEC WZ11TZIP,                                                        
//            DSIN=&&W11225X,                                                   
//            DSOUTZIP=W112.W112D1.W11225X(+1),                                 
//            ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                     
//            CONTENT=W11225X.CSV                                               
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W112.W112D1.W11225X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=kit%YYYYMMDD.ZIP                                            
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W112J024                                         
