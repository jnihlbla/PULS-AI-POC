//W116J184 JOB (640W1160100W116J184,W100),'RTN W116D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W116    EXEC W116P084,                                                        
//        INDIN=&W116..W116D1,                                                  
//        INDUT=&W116..W116D1                                                   
//*                                                                             
//W11684.W11684D1 DD DSN=&INDRTE..CONSTANT(W116DC02),DISP=SHR                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J184                                         
