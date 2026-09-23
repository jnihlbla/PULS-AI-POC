//W910J014 JOB (640W9100100W910J014,W100),'RTN W910D1',                         
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
//             DSIN=W910.W910D1.W91014(+0)                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W910J014                                         
