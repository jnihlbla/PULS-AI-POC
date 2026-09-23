//W551B7RE JOB (640W5510100W551B7RE,W100),'RTN W551B7',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
// EXEC WZ14PDAP                                                                
//SYSIN           DD *                                                          
W551B7-001                                                                      
W551B7                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551B7RE                                         
