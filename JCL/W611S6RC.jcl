//W611S6RC JOB (64091966000W611S6RC,W100),'RTN W611S6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//TST     EXEC WWAITTST,ROUTINE=WCARPS03,WAITFOR=ARCTRANS                       
// IF (TST.LIST.RC = 0) THEN  -- VI VÄNTAR FORTFARANDE PÅ SYSOUT                
//*--------------------------------------------------------------               
//TOMFIL  EXEC PGM=V16266,PARM='DD'                                             
//DD1     DD   DSN=WIN.W611S6.ARCTRANS,DISP=OLD,                                
//             RECFM=VB,LRECL=251                                               
//*                                                                             
//RCVE    EXEC W001PTSO                                                         
PROFILE PREFIX(W0SOP01)                                                         
RECEIVE                                                                         
DATASET('WIN.W611S6.ARCTRANS') MGMTCLAS(NOBACKUP)                               
END                                                                             
//*                                                                             
//EMPTST  EXEC WEMPTST,DSIN=WIN.W611S6.ARCTRANS                                 
//ARCDATA IF (EMPTST.T.RC = 0 ) THEN  -- FILEN INNEHÅLLER DATA                  
//*                                                                             
//COPY    EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSUT1   DD  DSN=WIN.W611S6.ARCTRANS,DISP=OLD                                 
//SYSUT2   DD  DSN=WIN.W611S6.W61194(+1),DISP=(NEW,CATLG,DELETE),               
//             SPACE=(251,(5000,2000),RLSE),AVGREC=U,                           
//             MGMTCLAS=BACKUPC                                                 
//SYSIN    DD  DUMMY                                                            
//*                                                                             
//TRIGG   EXEC WWAITOK,ROUTINE=WCARPS03,WAITFOR=ARCTRANS,                       
//             SOPGRP=PROD,SOPCMD=END                                           
//ARCDATA ENDIF                                                                 
//*                                                                             
//*--------------------------------------------------------------               
//  ENDIF                                                                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611S6RC                                         
//*                                                                             
