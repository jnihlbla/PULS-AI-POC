//W116J190 JOB (640W1160100W116J190,W100),'RTN W116D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W116    EXEC W116P090,                                                        
//             INDIN1=W116.W116D1.W11610(+0),                                   
//             INDIN2=W116.W116D1.W11612(+0),                                   
//             INDUT=W116.W116D1                                                
//*                                                                             
//SORT01.SORTIN DD                                                              
//              DD                                                              
//              DD DSN=W116.W116D1.W11689(+0),DISP=SHR                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J190                                         
