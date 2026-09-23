//W428J135 JOB (670W4280100W428J135,W100),'RTN W428D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//W015    EXEC W015P036,                                                        
//             DSIN=W428.W428D3.W42833(+0)                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W428J135                                         
