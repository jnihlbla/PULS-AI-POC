//W561J032 JOB (670W5610100W561J032,W100),'RTN W561D6',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W561    EXEC W561P032                                                         
//*                                                                             
// EXEC WZ14PDAP,DSIN=W561.W561D6.W56132(+1)                                    
//SYSIN           DD *                                                          
W56132-001                                                                      
CN                                                                              
//*                                                                             
// EXEC WZ14PDAP,DSIN=W561.W561D6.W56133(+1)                                    
//SYSIN           DD *                                                          
W56132-001                                                                      
US                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W561J032                                         
