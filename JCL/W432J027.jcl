//W432J027 JOB (670W4320100W432J027,W100),'RTN W432V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//WZ14    EXEC WZ14DAP2,DSIN=W432.W432V1.W43226(+0),CPU=3                       
//SYSIN        DD *                                                             
W43226-001                                                                      
PDF                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W432J027                                         
