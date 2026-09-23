//W116J115 JOB (640W1160100W116J115,W100),'RTN W116D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W116    EXEC W116P015,                                                        
//             INDIN=W116.W116D6,                                               
//             INDUT=W116.W116D6                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J115                                         
