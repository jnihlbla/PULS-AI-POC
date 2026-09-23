//W970J009 JOB (640W0000100W970J009,W100),'RTN W970V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//BLKFKT EXEC WEZTP,                                                            
//             LIB=W.QASE.EPLUS,                                                
//             MEMBER=W9700900                                                  
//ACF2      DD DSN=W970.W970B1.W97002(+0),DISP=SHR                              
//USERS     DD DSN=W970.W970B1.W97004(+0),DISP=SHR                              
//HIXAR     DD DSN=W970.W970B1.W97006(+0),DISP=SHR                              
//USERTOT   DD DSN=W970.W970B1.W97009(+1),                                      
//             DISP=(MOD,CATLG,DELETE),                                         
//             DCB=(RECFM=FB,LRECL=81,BLKSIZE=6156),                            
//             SPACE=(TRK,(10,10),RLSE),MGMTCLAS=NOBACKUP                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W970J009                                         
