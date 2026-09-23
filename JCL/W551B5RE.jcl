//W551B5RE JOB (640W5510100W551B5RE,W100),'RTN W551B5',                         
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
W551B5-001                                                                      
W551B5                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551B5RE                                         
