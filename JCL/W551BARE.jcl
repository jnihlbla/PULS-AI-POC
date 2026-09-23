//W551BARE JOB (640W5510100W551BARE,W100),'RTN W551BA',                         
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
W551BA-001                                                                      
W551BA                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551BARE                                         
