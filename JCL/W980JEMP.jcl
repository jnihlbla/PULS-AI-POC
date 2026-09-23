//W980JEMP JOB (640W0090100W980JEMP,W100),'RTN W980D1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ   LOCAL                                                           
/*ROUTE PRINT LOCAL                                                             
//W980    EXEC PGM=V16266,PARM='DD'                                             
//*                                                                             
//DD1   DD  DSN=W980.W980D1.W98029(+1),DISP=(NEW,CATLG,DELETE),                 
//          DCB=(RECFM=FB,LRECL=16),                                            
//          MGMTCLAS=NOBACKUP,                                                  
//          SPACE=(16,(100,100)),AVGREC=U                                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W980JEMP                                         
