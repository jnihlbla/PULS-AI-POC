//W612J045 JOB (670W6120100W612J045,W100),'RTN W612V7',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W015    EXEC W015P033,                                                        
//             DSIN=W612.W612V7.W61245(+0)                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J045                                         
