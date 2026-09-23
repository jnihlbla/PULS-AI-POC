//WDMRJ001 JOB (640W0030200WDMRJ001,W100),'RTN WDMRV1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ NJEVC                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//PROCLIST EXEC FMEMBLST,LIB='W.PROD.PROCLIB'                                   
//SYSPRINT  DD DSN=WDMR.WDMRV1.WDMR01(+1),DISP=(NEW,CATLG,DELETE),              
//             MGMTCLAS=BACKUPC,DATACLAS=PSEN,                                  
//             RECFM=FB,LRECL=132                                               
//SYSIN    DD *                                                                 
≈≈FILEM DSP MEMBER=W*                                                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJ001                                         
