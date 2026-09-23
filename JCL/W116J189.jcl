//W116J189 JOB (640W1160100W116J189,W100),'RTN W116D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0,LINES=999                                         
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W116    EXEC W116P089,                                                        
//        INDIN=&W116..W116D1,                                                  
//        INDUT=&W116..W116D1                                                   
//*                                                                             
//W11689.W11689D1 DD DSN=&INDRTE..CONSTANT(W116DC02),DISP=SHR                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J189                                         
