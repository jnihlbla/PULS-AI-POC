//W271D9MT   JOB (650W2710100W271D9MT,W100),'RTN W271D9',                       
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
//*                                                                             
//COPY  EXEC PGM=ICEGENER                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSUT1 DD  DSN=W271.W271D9.W27137D2,DISP=SHR                                  
//       DD  DSN=W271.W271D9.W27137DB,DISP=SHR                                  
//       DD  DSN=W271.W271D9.W27137V2,DISP=SHR                                  
//       DD  DSN=W271.W271D9.W27131V6,DISP=SHR                                  
//SYSUT2  DD DSN=W271.TOMTST.W27137(+1),DISP=(NEW,CATLG,DELETE),                
//           DCB=(BLKSIZE=27972),                                               
//           SPACE=(20,(10,5),RLSE),AVGREC=K,                                   
//           MGMTCLAS=DEL2                                                      
//SYSIN    DD  DUMMY                                                            
//TST21  EXEC WEMPTST,DSIN=W271.TOMTST.W27137(+1)                               
//ACT    EXEC WSOP,COND=(0,LT,TST21.T),COMMAND='ACTIVATE W271J037'              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W271D9MT                                         
