//W612J060 JOB (640W6120100W612J060,W100),'RTN W612D4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W612    EXEC W612P060                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J060                                         
