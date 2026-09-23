//W020J009 JOB (640W0200100W020J009,W100),'RTN W020D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W020    EXEC W020P009                                                         
//*                                                                             
//SORT01   EXEC PGM=SORT                                                        
//SYSOUT   DD  SYSOUT=*                                                         
//SYSIN    DD  DSN=&INDRTE..CONSTANT(W020PD71),DISP=SHR                         
//SORTIN   DD  DSN=&&W02009X,DISP=(OLD,DELETE,DELETE)                           
//SORTOUT  DD  DSN=&&W02009S,                                                   
//             DISP=(NEW,PASS,DELETE),                                          
//             DATACLAS=PSEN,MGMTCLAS=DEL10                                     
//*                                                                             
//TZIP1  EXEC WZ11TZIP,                                                         
//           DSIN=&&W02009S,                                                    
//           DSOUTZIP=W020.W020D1.W02009X(+1),                                  
//           ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                      
//           CONTENT=W02009X.CSV                                                
//*                                                                             
//WQSEN1  EXEC WZ11P023,                                                        
//             DSIN=W020.W020D1.W02009X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=srspart2009v2%YYYYMMDD.ZIP                                  
//*                                                                             
//TZIP2   EXEC WZ11TZIP,                                                        
//            DSIN=&&W02031X,                                                   
//            DSOUTZIP=W020.W020D1.W02031X(+1),                                 
//            ZIPDISP=(NEW,CATLG,DELETE),ZIPMGMTC=DEL2BKPC,                     
//            CONTENT=W02031X.CSV                                               
//*                                                                             
//WQSEN2  EXEC WZ11P023,                                                        
//             DSIN=W020.W020D1.W02031X(+1)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.AZURE.DATA                                                  
¤MQMPROP PhysicalId=srspart2031v2%YYYYMMDD.ZIP                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W020J009                                         
