//W560JD82 JOB (650W5600100W560JD82,W100),'RTN W560R1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
// EXEC WZ14DAP4,DSIN=W560.W560R1.W56082(+0),CPU=3                              
//SYSIN           DD *                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W560JD82                                         
