//W612J057 JOB (640W6120100W612J057,W100),'RTN W612DC',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC JCLLIB ORDER=(W.QASE.PROCLIB)                                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W612     EXEC W612P057                                                        
//*                                                                             
//SORT1    EXEC PGM=SORT                                                        
//SYSOUT   DD  SYSOUT=*                                                         
//SYSIN    DD  DSN=&INDRTE..CONSTANT(W612PD55),DISP=SHR                         
//SORTIN   DD  DSN=W612.W612DC.W612571(+1),DISP=OLD                             
//*                                                                             
//SORTOUT  DD  DSN=&&W61257X,                                                   
//             DISP=(NEW,PASS,DELETE),                                          
//             DATACLAS=PSEN,MGMTCLAS=NOBACKUP                                  
//*                                                                             
//ZIP1     EXEC WZ11TZIP,                                                       
//           DSIN=&&W61257X,                                                    
//           DSOUTZIP=W612.W612DC.W61257X(+1),                                  
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=WDH711.CSV,ZIPDATAC=PSEB                                   
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=W612.W612DC.W61257X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=partsmeasurement%YYYYMMDD                                   
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W612J057                                         
