//W488J180 JOB (650W4880100W488J180,W100),'RTN W488D2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W488    EXEC W488P080                                                         
//W48880.W48880D1 DD DUMMY                                                      
//*                                                                             
//SORT1    EXEC PGM=SORT                                                        
//SYSOUT   DD  SYSOUT=*                                                         
//SYSIN    DD  DSN=&INDRTE..CONSTANT(W488PD01),DISP=SHR                         
//SORTIN   DD  DSN=&&W48881X,DISP=(OLD,DELETE,DELETE)                           
//*                                                                             
//SORTOUT  DD  DSN=&&W48881S,                                                   
//             DISP=(NEW,PASS,DELETE),                                          
//             DATACLAS=PSEN,MGMTCLAS=NOBACKUP                                  
//*                                                                             
//ZIP      EXEC WZ11TZIP,                                                       
//           DSIN=&&W48881S,                                                    
//           DSOUTZIP=W488.W488D2.W48882(+1),                                   
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W48881.CSV,ZIPDATAC=PSEB                                   
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=W488.W488D2.W48882(+1)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=buffer%YYYYMMDD                                             
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W488J180                                         
