//W463J077 JOB (670W4630100W463J077,W100),'RTN W463D4',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W015    EXEC W015P036,                                                        
//             DSIN=W463.W463D4.W46378(+0)                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463J077                                         
