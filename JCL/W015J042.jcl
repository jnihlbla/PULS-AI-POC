//W015J042 JOB (670W0020200W015J042,W100),'RTN W015D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
/*JOBPARM TIME=5,FORMS=1800,LINECT=0                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W01542  EXEC W015P042                                                         
//*                                                                             
//END     EXEC WSOPEND,PROCESS=W015J042                                         
//*                                                                             
