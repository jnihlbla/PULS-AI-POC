//W111J037 JOB (640W1110100W111J037,W100),'RTN W111B1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST9                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//EMPTYT1 EXEC WEMPTST,DSIN=W111.W111B1.W11137                                  
//*                                                                             
//A IF (EMPTYT1.T.RC = 0) THEN                                                  
//COPY    EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSUT1   DD  DSN=W111.W111B1.W11137,DISP=SHR                                  
//SYSUT2   DD  DSN=W111.W111B1.W11137C(+1),DISP=(NEW,CATLG,DELETE),             
//             MGMTCLAS=BACKUPC,DATACLAS=PSEN                                   
//SYSIN    DD  DUMMY                                                            
//A ELSE                                                                        
//CAN     EXEC WSOP                                                             
         CANCEL W111J038                                                        
         CANCEL W111J034                                                        
//A ENDIF                                                                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W111J037                                         
