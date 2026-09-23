//W612J037 JOB (670W6120100W612J037,W100),'RTN W612D3',                         
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
//             DSIN=W612.W612D3.W61237(+0)                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J037                                         
