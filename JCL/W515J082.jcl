//W515J082 JOB (650W5100100W515J082,W100),'RTN W515D4',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W515     EXEC W515P082                                                        
//*                                                                             
/W51582.W51578D1 DD DSN=W515.W515D4.W51574(+0),DISP=SHR                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W515.W515D4.W51582(+1)                                    
//SYSIN           DD *                                                          
W51582-001                                                                      
W51582                                                                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W515J082                                         
