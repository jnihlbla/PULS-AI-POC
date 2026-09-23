//W222J005 JOB (670W2220100W222J005,W100),'RTN W200V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST2                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W222     EXEC W222P005,                                                       
//             INDUT=W222.W200V1.W22205(+1)                                     
//*                                                                             
//* EXTRACT FILE - INCLUDE PARTS WHEN KVPB-SEP > 0                              
EXTRFIL1                                                                        
//*                                                                             
//ZIP     EXEC WZ11TZIP,                                                        
//           DSIN=&&W22205X,                                                    
//           DSOUTZIP=W222.W200V1.W222053(+1),                                  
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W22205X.CSV,ZIPDATAC=PSEB                                  
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W222.W200V1.W222053(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=demandinformation2128V2%YYYYMMDD.zip                        
/*                                                                              
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//COPY     EXEC PGM=V16459,PARM='ICEGENER/3/'                                   
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W222.W200V1.W22205(+1),DISP=SHR                              
//************                                                                  
//*                                                                             
//SYSUT2   DD  DSN=WXTR.PARTINFO.WREQWEEK(+1),                 -LIM(6)          
//             DISP=(NEW,CATLG,DELETE),                                         
//************ KOPIA AV W22205/WXTR.PARTINFO.WREQWEEK,                          
//             MGMTCLAS=NOBACKUP,DATACLAS=PSEB                                  
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//SOPEND  EXEC WSOPEND,PROCESS=W222J005                                         
