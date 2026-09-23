//W271J048 JOB (640W2710100W271J048,W100),'RTN W271D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P048                                                         
//*                                                                             
//SORT1.SORTIN DD DSN=W271.LDC.W27147(+0),DISP=SHR                              
//             DD DSN=W271.SDC.W27147(+0),DISP=SHR                              
//             DD DSN=W271.CHN.W27147(+0),DISP=SHR                              
//*                                                                             
//SORT2.SORTIN DD DSN=W271.LDC.W27147(-1),DISP=SHR                              
//             DD DSN=W271.SDC.W27147(-1),DISP=SHR                              
//             DD DSN=W271.CHN.W27147(-1),DISP=SHR                              
//*                                                                             
//W27148.W27148D5 DD DUMMY                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J048                                         
