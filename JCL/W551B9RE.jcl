//W551B9RE JOB (640W5510100W551B9RE,W100),'RTN W551B9',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//FREE    EXEC WFREE,NAME=W551B9,MAXRC=8                                        
//*                                                                             
// EXEC WZ14PDAP                                                                
//SYSIN           DD *                                                          
W551B9-001                                                                      
W551B9                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551B9RE                                         
