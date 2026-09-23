//W517J006 JOB (650W5170100W517J006,W100),'RTN W517V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//       EXEC W517P006                                                          
//*                                                                             
// EXEC WZ14PDAP,DSIN=W517.W517V1.W51706(+1)                                    
//SYSIN           DD *                                                          
W51706-001                                                                      
W51706                                                                          
//SOP     EXEC WSOPEND,PROCESS=W517J006                                         
