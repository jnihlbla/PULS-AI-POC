//W463J095 JOB (640W4630100W463J095,W100),'RTN W463V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC JCLLIB ORDER=(W.QASE.PROCLIB)                                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*                                                                             
//W463     EXEC W463P095                                                        
//*                                                                             
//*                                                                             
//TZIP    EXEC WZ11TZIP,                                                        
//            DSIN=W463.W463V2.W4639S(+1),                                      
//            DSOUTZIP=W463.W463V2.ZIP.W4639S(+1),                              
//            ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                     
//            CONTENT=W4639S.CSV                                                
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W463.W463V2.ZIP.W4639S(+1)                                  
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=DDGSscorecard%YYYYMMDD.ZIP                                  
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W463J095                                         
