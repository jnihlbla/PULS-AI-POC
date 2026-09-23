//W513J133 JOB (640W5130100W513J133,W100),'RTN W513D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W513    EXEC W513P033,                                                        
//             DEST1=LOCAL,DEST2=LOCAL,DEST3=LOCAL,                             
//             DEST4=LOCAL,DEST5=LOCAL,DEST9=LOCAL                              
//W51333.W51333D1 DD DSN=W513.W513D3.W51336                                     
//W51333.W51333DA DD DSN=W513.W513D3.W513O6(+1)                                 
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W513J133                                         
