//W475TOMF JOB (650W4750100W475TOMF,W100),'RTN W400D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//  EXEC PGM=V16266,PARM='DD'                                                   
//DD1   DD  DSN=W475.BMP.W47511(+1),DISP=(NEW,CATLG,DELETE),                    
//          DCB=(RECFM=VB,LRECL=1002,BLKSIZE=27998),                            
//          MGMTCLAS=NOBACKUP,                                                  
//          SPACE=(27998,(7,50),RLSE)                                           
//DD2   DD  DSN=W475.W475D1.W47511(+1),DISP=(NEW,CATLG,DELETE),                 
//          DCB=(RECFM=FB,LRECL=526,BLKSIZE=27878),                             
//          MGMTCLAS=NOBACKUP,                                                  
//          SPACE=(27878,(7,50),RLSE)                                           
//DD3   DD  DSN=W475.BMP.W47521(+1),DISP=(NEW,CATLG,DELETE),                    
//          DCB=(RECFM=VB,LRECL=1002,BLKSIZE=27998),                            
//          MGMTCLAS=NOBACKUP,                                                  
//          SPACE=(27998,(7,50),RLSE)                                           
//DD4   DD  DSN=W475.W475D1.W47521(+1),DISP=(NEW,CATLG,DELETE),                 
//          DCB=(RECFM=VB,LRECL=264,BLKSIZE=27984),                             
//          MGMTCLAS=NOBACKUP,                                                  
//          SPACE=(27984,(7,50),RLSE)                                           
//DD5   DD  DSN=W475.BMP.W47541(+1),DISP=(NEW,CATLG,DELETE),                    
//          DCB=(RECFM=VB,LRECL=1002,BLKSIZE=27998),                            
//          MGMTCLAS=NOBACKUP,                                                  
//          SPACE=(27998,(7,50),RLSE)                                           
//DD6   DD  DSN=W475.W475D1.W47541(+1),DISP=(NEW,CATLG,DELETE),                 
//          DCB=(RECFM=VB,LRECL=530,BLKSIZE=27998),                             
//          MGMTCLAS=NOBACKUP,                                                  
//          SPACE=(27998,(7,50),RLSE)                                           
//DD7   DD  DSN=W475.W475S1.W47553(+1),DISP=(NEW,CATLG,DELETE),                 
//          DCB=(RECFM=VB,LRECL=625,BLKSIZE=27998),                             
//          MGMTCLAS=NOBACKUP,                                                  
//          SPACE=(27998,(7,50),RLSE)                                           
//SOP     EXEC WSOPEND,PROCESS=W475TOMF                                         
