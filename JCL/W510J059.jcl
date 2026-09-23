//W510J059 JOB (650W5100100W510J059,W100),'RTN W500M1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W510   EXEC W510P059                                                          
//*                                                                             
// EXEC WZ14PDAP,DSIN=W510.W500M1.W51059(+1)                                    
//SYSIN           DD *                                                          
W51059-001                                                                      
W51059                                                                          
//SOP     EXEC WSOPEND,PROCESS=W510J059                                         
