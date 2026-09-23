//W478JD88 JOB (650W4780100W478JD88,W100),'RTN W478V2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
// EXEC WZ14DAP4,DSIN=W478.W478V2.W47888A(+0),CPU=3                             
//SYSIN           DD *                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W478JD88                                         
