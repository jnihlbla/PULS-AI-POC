//W511J193 JOB (650W5110100W511J193,W100),'RTN W500V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
// EXEC WZ14PDAP,DSIN=WUT.W500V1.W51193(+0)                                     
//SYSIN           DD *                                                          
W51193-001                                                                      
W5119300                                                                        
//SOP     EXEC WSOPEND,PROCESS=W511J193                                         
