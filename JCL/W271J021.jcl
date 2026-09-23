//W271J021 JOB (640W2710100W271J021,W100),'RTN W271D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P021,                                                        
//             INDUT=W271.W271D1                                                
//*                                                                             
//SORT.SORTIN  DD DSN=W271.LDC.W27148(+0),DISP=SHR                              
//             DD DSN=W271.SDC.W27148(+0),DISP=SHR                              
//             DD DSN=W271.CHN.W27148(+0),DISP=SHR                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J021                                         
