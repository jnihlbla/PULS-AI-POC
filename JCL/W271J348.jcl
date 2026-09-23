//W271J348 JOB (640W2710100W271J348,W100),'RTN W271DA',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P048,                                                        
//             INDUT1=W271.LDC,                                                 
//             INDUT2=W271.SDC,                                                 
//             INDUT3=W271.NDC                                                  
//*                                                                             
//*                                                                             
//SORT1.SORTIN DD DSN=W271.NDC.W27147(+0),DISP=SHR                              
//*                                                                             
//SORT2.SORTIN DD DSN=W271.NDC.W27147(-1),DISP=SHR                              
//*                                                                             
//W27148.W27148D3 DD DUMMY                                                      
//W27148.W27148D4 DD DUMMY                                                      
//W27148.W27148D6 DD DUMMY                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J348                                         
