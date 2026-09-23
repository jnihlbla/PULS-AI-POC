//W011J060 JOB (640W0110100W011J060,W100),'RTN W011D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W011    EXEC W011P060                                                         
//*                                                                             
//SORT6    EXEC PGM=SORT                                                        
//SYSOUT   DD  SYSOUT=*                                                         
//SYSIN    DD  DSN=&INDRTE..CONSTANT(W011PD6X),DISP=SHR                         
//SORTIN   DD  DSN=&&W01164X,DISP=(OLD,DELETE,DELETE)                           
//*                                                                             
//SORTOUT  DD  DSN=&&W01164S,                                                   
//             DISP=(NEW,PASS,DELETE),                                          
//             DATACLAS=PSEN,MGMTCLAS=NOBACKUP                                  
//*                                                                             
//TZIP2   EXEC WZ11TZIP,                                                        
//           DSIN=&&W01164S,                                                    
//           DSOUTZIP=&W011..&RTEGRP..W01164X(+1),                              
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W01164X.CSV                                                
//*                                                                             
//SORT7    EXEC PGM=SORT                                                        
//SYSOUT   DD  SYSOUT=*                                                         
//SYSIN    DD  DSN=&INDRTE..CONSTANT(W011PD6X),DISP=SHR                         
//SORTIN   DD  DSN=&&W01166X,DISP=(OLD,DELETE,DELETE)                           
//*                                                                             
//SORTOUT  DD  DSN=&&W01166S,                                                   
//             DISP=(NEW,PASS,DELETE),                                          
//             DATACLAS=PSEN,MGMTCLAS=NOBACKUP                                  
//*                                                                             
//TZIP3   EXEC WZ11TZIP,                                                        
//            DSIN=&&W01166S,                                                   
//            DSOUTZIP=&W011..&RTEGRP..W01166X(+1),                             
//            ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                     
//            CONTENT=W01166X.CSV,ZIPDATAC=PSEB                                 
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=&W011..&RTEGRP..W01166X(+1)                                 
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=CDCrefill626%YYYYMMDD.zip                                   
/*                                                                              
//*                                                                             
//SORT8    EXEC PGM=SORT                                                        
//SYSOUT   DD  SYSOUT=*                                                         
//SYSIN    DD  DSN=&INDRTE..CONSTANT(W011PD6X),DISP=SHR                         
//SORTIN   DD  DSN=&&W01169X,DISP=(OLD,DELETE,DELETE)                           
//*                                                                             
//SORTOUT  DD  DSN=&&W01169S,                                                   
//             DISP=(NEW,PASS,DELETE),                                          
//             DATACLAS=PSEN,MGMTCLAS=NOBACKUP                                  
//*                                                                             
//TZIP4   EXEC WZ11TZIP,                                                        
//            DSIN=&&W01169S,                                                   
//            DSOUTZIP=&W011..&RTEGRP..W01169X(+1),                             
//            ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                     
//            CONTENT=W01169X.CSV,ZIPDATAC=PSEB                                 
//*                                                                             
//WQSEN2  EXEC WZ11P023,                                                        
//             DSIN=&W011..&RTEGRP..W01169X(+1)                                 
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=CDCrefill629%YYYYMMDD.zip                                   
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011J060                                         
