//W116J160 JOB (640W1160100W116J160,W100),'RTN W116D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W116    EXEC W116P060,                                                        
//             INDREG=W116.W11679(+0),                                          
//             INDUT=W116.W116D4.W11661B(+1)                                    
//*                                                                             
//W11660.W11660D3 DD DSN=&INDRTE..CONSTANT(W116CN),DISP=SHR                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J160                                         
