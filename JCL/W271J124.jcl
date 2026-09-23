//W271J124 JOB (640W2710100W271J124,W100),'RTN W271V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* NOTE!                                                                       
//*   THE PROGRAM W27124 IS RUN HERE TO                                         
//*    1) READ THE W27120 WEEKLY PRODUCED IN THIS ROUTINE                       
//*    AND                                                                      
//*    2) CREATE THE W27120X FILE, SO THAT THE W27120X FILE                     
//*       CAN THEN BE SENT TO THE AZURE DATALAKE                                
//*                                                                             
//*   SINCE THERE IS NO DAILY FILE TO COMPARE WITH IN THIS ROUTINE              
//*   THE INDIN1 SORTIN FILE IS OVERRIDDEN TO NULLFILE                          
//*                                                                             
//W271    EXEC W271P024,                                                        
//             INDIN1=W271.W271V1                                               
//SORT1.SORTIN DD DSN=NULLFILE,RECFM=FB,LRECL=30                                
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
//SOPEND  EXEC WSOPEND,PROCESS=W271J124                                         
