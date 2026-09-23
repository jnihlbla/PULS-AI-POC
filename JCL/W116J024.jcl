//W116J024 JOB (640W1160100W116J024,W100),'RTN W116D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W116    EXEC W116P024,                                                        
//             INDIN=W116.W116D3,                                               
//             INDUT=W116.W116D3                                                
//*                                                                             
//W11624.W11624D1 DD DSN=&INDRTE..CONSTANT(W1162401),DISP=SHR                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J024                                         
