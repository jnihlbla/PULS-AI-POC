//W219J016 JOB (640W2190100W219J016,W100),'RTN W219V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
//      INCLUDE MEMBER=SYSTÖ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W219    EXEC W219P016                                                         
//TSO.SYSTSIN DD *                                                              
DSN SYS(D2G0)                                                                   
RUN PROG(W21916) PLAN (W21916) LIB('W.QASE.LOAD')                               
END                                                                             
//*                                                                             
//TZIP    EXEC WZ11TZIP,                                                        
//            DSIN=WXTR.W219V1.W21916(+1),                                      
//            DSOUTZIP=WXTR.W219V1.W21916X(+1),                                 
//            ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                     
//            CONTENT=W21916X.CSV                                               
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=WXTR.W219V1.W21916X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=CampaignParts%YYYYMMDD.ZIP                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W219J016                                         
