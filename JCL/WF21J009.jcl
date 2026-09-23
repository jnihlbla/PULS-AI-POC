//WF21J009 JOB (640WF210100WF21J009,W100),'RTN WF21S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTF                                                    
//*+JBS BIND IMG0                                                               
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WF2109  EXEC WF21P009                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF21J009                                         
