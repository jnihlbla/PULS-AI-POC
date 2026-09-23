//W970J004 JOB (640W0000100W970J004,W100),'RTN W970V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//ICVALJ EXEC WEZTP,                                                            
//             LIB=W.QASE.EPLUS,                                                
//             MEMBER=W9700400                                                  
//EZTVFM    DD SPACE=(CYL,(40,10))                                              
//INFILA    DD DSN=W970.W970B1.W97002(+0),DISP=SHR                              
//INFILU    DD DSN=W970.W970B1.W97003(+0),DISP=SHR                              
//UTFIL     DD DSN=W970.W970B1.W97004(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             DCB=(RECFM=FB,LRECL=156,BLKSIZE=6240),                           
//             SPACE=(TRK,(50,10),RLSE),MGMTCLAS=NOBACKUP                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W970J004                                         
