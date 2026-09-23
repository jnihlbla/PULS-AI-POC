//W416J003 JOB (670W4160100W416J003,W100),'RTN W416V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W416    EXEC W416P003                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W416J003                                         
