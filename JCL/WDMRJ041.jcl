//WDMRJ041 JOB (640W0030200WDMRJ041,W100),'RTN WDMRV2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ   NJEVC                                                           
/*ROUTE PRINT NJOVC                                                             
//*                                                                             
//*                                                                             
//PROCLIST EXEC FMEMBLST,LIB='W.PROD.JCL'                                       
//SYSPRINT  DD DSN=WDMR.WDMRV2.WDMR41(+1),DISP=(NEW,CATLG,DELETE),              
//             MGMTCLAS=DEL2BKPC,DATACLAS=PSEN,                                 
//             RECFM=FB,LRECL=132                                               
//SYSIN    DD *                                                                 
≈≈FILEM DSP MEMBER=W*                                                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJ041                                         
