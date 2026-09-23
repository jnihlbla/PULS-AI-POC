//W560JD85 JOB (650W5600100W560JD85,W100),'RTN W560Y3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
// EXEC WZ14DAP4,DSIN=W560.W560Y3.W56085(+0),CPU=3                              
//SYSIN           DD *                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W560JD85                                         
