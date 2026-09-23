//W413D4ME JOB (670W4130100W413D4ME,W100),'RTN W413D4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTZ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
// EXEC WZ14PDAP,DSIN=W413.W413D4.W41333(+0)                                    
//SYSIN           DD *                                                          
W41332-001                                                                      
W41332                                                                          
//*                                                                             
// EXEC WZ14PDAP,DSIN=W413.W413D4.W41334(+0)                                    
//SYSIN           DD *                                                          
W41332-002                                                                      
W41332                                                                          
//*                                                                             
// EXEC WZ14PDAP,DSIN=W413.W413D4.W41335(+0)                                    
//SYSIN           DD *                                                          
W41332-003                                                                      
W41332                                                                          
//*                                                                             
// EXEC WZ14PDAP,DSIN=W413.W413D4.W41336(+0)                                    
//SYSIN           DD *                                                          
W41332-004                                                                      
W41332                                                                          
//*                                                                             
// EXEC WZ14PDAP,DSIN=W413.W413D4.W41337(+0)                                    
//SYSIN           DD *                                                          
W41332-005                                                                      
W41332                                                                          
//*                                                                             
// EXEC WZ14PDAP,DSIN=W413.W413D4.W41338(+0)                                    
//SYSIN           DD *                                                          
W41332-006                                                                      
W41332                                                                          
//*                                                                             
// EXEC WZ14PDAP,DSIN=W413.W413D4.W41339(+0)                                    
//SYSIN           DD *                                                          
W41332-007                                                                      
W41332                                                                          
//*                                                                             
// EXEC WZ14PDAP,DSIN=W413.W413D4.W41340(+0)                                    
//SYSIN           DD *                                                          
W41332-008                                                                      
W41332                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W413D4ME                                         
