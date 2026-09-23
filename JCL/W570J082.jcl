//W570J082 JOB (650W5700100W570J082,W100),'RTN W570D4',                         
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
//W570     EXEC W570P082                                                        
//*                                                                             
// EXEC WZ14PDAP,DSIN=W570.W570D4.W57082(+1)                                    
//SYSIN           DD *                                                          
W57082-001                                                                      
W57082                                                                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W570J082                                         
