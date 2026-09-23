//W612J027 JOB (670W6120100W612J027,W100),'RTN W612DB',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W612    EXEC W612P027                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J027                                         
