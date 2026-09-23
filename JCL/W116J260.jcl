//W116J260 JOB (640W1160100W116J260,W100),'RTN W116D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W116    EXEC W116P060,                                                        
//             INDIN2=&W116..W116D2,                                            
//             INDREG=&W116..W116D2,                                            
//             INDUT=&W116..W116D2                                              
//W11660.W11660D3 DD  DSN=&INDRTE..CONSTANT(W116NDC2),DISP=SHR                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J260                                         
