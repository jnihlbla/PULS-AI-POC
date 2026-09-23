//WF23JD01 JOB (670WF230100WF23JD01,W100),'RTN WF23S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
// EXEC WZ14DAP4,DSIN=WF23.WF23S1.WF2301(+0),CPU=3                              
//SYSIN           DD *                                                          
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF23JD01                                         
