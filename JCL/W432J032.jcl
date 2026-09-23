//W432J032 JOB (650W4320100W432J032,W100),'RTN W432D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W432    EXEC W432P032                                                         
//*                                                                             
//TZIP    EXEC WZ11TZIP,                                                        
//            DSIN=&&W43232X,                                                   
//            DSOUTZIP=W432.W432D1.W43232X(+1),                                 
//            ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                     
//            CONTENT=W43232X.CSV                                               
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W432.W432D1.W43232X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=customerv2%YYYYMMDD.ZIP                                     
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W432J032                                         
