//W612J050 JOB (640W6120100W612J050,W100),'RTN W612D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W612    EXEC W612P050                                                         
//*                                                                             
//SORT1    EXEC PGM=SORT                                                        
//SYSOUT   DD  SYSOUT=*                                                         
//SYSIN    DD  DSN=&INDRTE..CONSTANT(W011PD60),DISP=SHR                         
//SORTIN   DD  DSN=&&W61250X1,DISP=(OLD,DELETE,DELETE)                          
//*                                                                             
//SORTOUT  DD  DSN=&&W61250S1,                                                  
//             DISP=(NEW,PASS,DELETE),                                          
//             DATACLAS=PSEN,MGMTCLAS=NOBACKUP                                  
//*                                                                             
//SORT2    EXEC PGM=SORT                                                        
//SYSOUT   DD  SYSOUT=*                                                         
//SYSIN    DD  DSN=&INDRTE..CONSTANT(W011PD60),DISP=SHR                         
//SORTIN   DD  DSN=&&W61250X2,DISP=(OLD,DELETE,DELETE)                          
//*                                                                             
//SORTOUT  DD  DSN=&&W61250S2,                                                  
//             DISP=(NEW,PASS,DELETE),                                          
//             DATACLAS=PSEN,MGMTCLAS=NOBACKUP                                  
//*                                                                             
//ZIP1     EXEC WZ11TZIP,                                                       
//           DSIN=&&W61250S1,                                                   
//           DSOUTZIP=W612.W612D4.W61250X1(+1),                                 
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=WDL611.CSV,ZIPDATAC=PSEB                                   
//*                                                                             
//ZIP2     EXEC WZ11TZIP,                                                       
//           DSIN=&&W61250S2,                                                   
//           DSOUTZIP=W612.W612D4.W61250X2(+1),                                 
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=WDL611.CSV,ZIPDATAC=PSEB                                   
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=W612.W612D4.W61250X1(+1)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=inboundR32V2%YYYYMMDD                                       
/*                                                                              
//WQSEN2  EXEC WZ11P023,                                                        
//             DSIN=W612.W612D4.W61250X2(+1)                                    
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=inboundRXXV2%YYYYMMDD                                       
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W612J050                                         
