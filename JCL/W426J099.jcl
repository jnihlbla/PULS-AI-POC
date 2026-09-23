//W426J099 JOB (670W4260100W426J099,W100),'RTN W426V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W015    EXEC W015P033,                                                        
//             DSIN=W426.W426V1.W42699(+0)                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W426J099                                         
