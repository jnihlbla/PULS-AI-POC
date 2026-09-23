//WF21JD11 JOB (640WF210100WF21JD11,W100),'RTN WF21S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
// EXEC WZ14DAP4,DSIN=WF21.WF21S1.WF2111(+0),CPU=3                              
//SYSIN           DD *                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF21JD11                                         
