//W116J090 JOB (640W1160100W116J090,W100),'RTN W116D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W116    EXEC W116P090,                                                        
//             INDIN1=W116.W116D4.W11627(+0),                                   
//             INDIN2=W116.W116D4.W11689(+0),                                   
//             INDUT=W116.W116D4                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J090                                         
