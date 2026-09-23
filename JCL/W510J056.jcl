//W510J056 JOB (650W5100100W510J056,W100),'RTN W500M1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W510   EXEC W510P056                                                          
//*                                                                             
// EXEC WZ14PDAP,DSIN=W510.W500M1.W51056(+1)                                    
//SYSIN           DD *                                                          
W51056-001                                                                      
W51056                                                                          
//SOP     EXEC WSOPEND,PROCESS=W510J056                                         
