//W271J121 JOB (640W2710100W271J121,W100),'RTN W271V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P021,                                                        
//             INDUT=W271.W271V1                                                
//*                                                                             
//*                                                                             
//SORT.SORTIN  DD DSN=W271.LDC.W27147(+0),DISP=SHR                              
//             DD DSN=W271.SDC.W27147(+0),DISP=SHR                              
//             DD DSN=W271.NDC.W27147(+0),DISP=SHR                              
//             DD DSN=W271.CHN.W27147(+0),DISP=SHR                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J121                                         
