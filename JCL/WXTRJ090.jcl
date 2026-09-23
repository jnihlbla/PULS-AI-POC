//WXTRJ090 JOB (640W0001000WXTRJ090,W100),'RTN WXTRD2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//WXTR    EXEC WXTRP090,                                                        
//             CNTRL=WDAYCARD,                                                  
//             INDIN=W011.QASE,                                                 
//             INDUT=WXTR.WXTRD2                                                
//*                                                                             
//SORT03.SYSIN    DD DSN=W.PROD.CONSTANT(WXTRPD90),DISP=SHR                     
//*                                                                             
//WXTR90.WXTR90D9 DD DSN=WXTR.WXTRD2.WXTR9A(+1)                                 
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WXTRJ090                                         
