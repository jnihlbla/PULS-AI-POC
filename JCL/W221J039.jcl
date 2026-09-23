//W221J039 JOB (670W2210100W221J039,W100),'RTN W218V1',                         
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
//             DSIN=W221.W218V1.W22139(+0)                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J039                                         
