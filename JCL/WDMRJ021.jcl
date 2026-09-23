//WDMRJ021 JOB (640W0030200WDMRJ021,W100),'RTN WDMRV6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//COBOLLST EXEC FMEMBLST,LIB='W.PROD.COBOL'                                     
//SYSPRINT  DD DSN=WDMR.WDMRV6.WDMR21(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             MGMTCLAS=NOBACKUP,DATACLAS=PSEN,                                 
//             RECFM=FB,LRECL=132                                               
//SYSIN    DD *                                                                 
≈≈FILEM DSP MEMBER=W*                                                           
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WDMRJ021                                         
