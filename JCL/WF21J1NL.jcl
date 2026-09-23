//WF21J1NL JOB (670WF210100WF21J1NL,W100),'RTN WF21S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTF                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//WF21    EXEC WF21P010                                                         
//*                                                                             
NLXXXXXXXXX                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF21J1NL                                         
