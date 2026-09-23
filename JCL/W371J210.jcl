//W371J210 JOB (540W3710100W371J210,W100),'RTN W371V2 ',                        
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM TIME=0,LINES=15,CARDS=0,FORMS=1800,LINECT=00                          
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//*                                                                             
//COPY     EXEC PGM=V16459,PARM='ICEGENER/3/'                                   
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W371.W371V2.W3714A,DISP=SHR                                  
//************ IN-REG                                                           
//*                                                                             
//SYSUT2   DD  DSN=W371.W371P2.P.W3714A(+1),DISP=(NEW,CATLG,DELETE),            
//************ KOPIA AV                                                         
//*            SPACE=(22,(20,5),RLSE),AVGREC=K,                                 
//             DATACLAS=PSEB,                                                   
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//*                                                                             
//COPY     EXEC PGM=V16459,PARM='ICEGENER/3/'                                   
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W371.W371V2.W3714B,DISP=SHR                                  
//************ IN-REG                                                           
//*                                                                             
//SYSUT2   DD  DSN=W371.W371P2.P.W3714B(+1),DISP=(NEW,CATLG,DELETE),            
//************ KOPIA AV                                                         
//*            SPACE=(10,(20,5),RLSE),AVGREC=K,                                 
//             DATACLAS=PSEN,                                                   
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//*                                                                             
//COPY     EXEC PGM=V16459,PARM='ICEGENER/3/'                                   
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W371.W371V2.W3714C,DISP=SHR                                  
//************ IN-REG                                                           
//*                                                                             
//SYSUT2   DD  DSN=W371.W371P2.P.W3714C(+1),DISP=(NEW,CATLG,DELETE),            
//************ KOPIA AV                                                         
//             SPACE=(10,(20,5),RLSE),AVGREC=K,                                 
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//*                                                                             
//COPY     EXEC PGM=V16459,PARM='ICEGENER/3/'                                   
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W371.W371V2.W3714D,DISP=SHR                                  
//************ IN-REG                                                           
//*                                                                             
//SYSUT2   DD  DSN=W371.W371P2.P.W3714D(+1),DISP=(NEW,CATLG,DELETE),            
//************ KOPIA AV                                                         
//             SPACE=(9,(20,5),RLSE),AVGREC=K,                                  
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//*                                                                             
//COPY     EXEC PGM=V16459,PARM='ICEGENER/3/'                                   
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W371.W371V2.W3714E,DISP=SHR                                  
//************ IN-REG                                                           
//*                                                                             
//SYSUT2   DD  DSN=W371.W371P2.P.W3714E(+1),DISP=(NEW,CATLG,DELETE),            
//************ KOPIA AV                                                         
//             SPACE=(11,(20,5),RLSE),AVGREC=K,                                 
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J210                                         
