//WDMRJ071 JOB (640W0030200WDMRJ071,W100),'RTN WDMRV4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ NJEVC                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//PROCLIST EXEC FMEMBLST,LIB='W.PROD.DBD'                                       
//SYSPRINT DD  DSN=WDMR.WDMRV4.WDMR71(+1),DISP=(NEW,CATLG,DELETE),              
//             MGMTCLAS=DEL2BKPC,DATACLAS=PSEN,                                 
//             RECFM=FB,LRECL=132                                               
//SYSIN    DD *                                                                 
≈≈FILEM DSP MEMBER=W*                                                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJ071                                         
