//W414J001 JOB (640W4120100W414J001,W100),'RTN W414D1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W414    EXEC W414P001                                                         
//*                                                                             
//EMPTYT1 EXEC WEMPTST,DSIN=W414.W414D1.W41418(+1)                              
//*                                                                             
//    IF (EMPTYT1.T.RC > 0) THEN                                                
//      EXEC PGM=IEFBR14                                                        
/ DD       DSN=W414.W414D1.W41418(+1),DISP=(OLD,DELETE)                         
//    ELSE                                                                      
//COPY    EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W414.W414D1.W41418(+1),DISP=SHR                              
//SYSUT2   DD  DSN=WXTR.VORLOG.DAILY(+1),                                       
//             DISP=(NEW,CATLG,DELETE),                                         
//             MGMTCLAS=BACKUPC,DATACLAS=PSEN                                   
//    ENDIF                                                                     
//*                                                                             
//EMPTYT2 EXEC WEMPTST,DSIN=W414.W414D1.W41419(+1)                              
//*                                                                             
//    IF (EMPTYT2.T.RC > 0) THEN                                                
//      EXEC PGM=IEFBR14                                                        
/ DD       DSN=W414.W414D1.W41419(+1),DISP=(OLD,DELETE)                         
//    ELSE                                                                      
//COPY2   EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W414.W414D1.W41419(+1),DISP=SHR                              
//SYSUT2   DD  DSN=WXTR.XDCLOG.DAILY(+1),                                       
//             DISP=(NEW,CATLG,DELETE),                                         
//             MGMTCLAS=BACKUPC,DATACLAS=PSEN                                   
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W414J001                                         
