//W272J021 JOB (670W2720100W272J021,W100),'RTN W271D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W272    EXEC W272P021,                                                        
//             INDIN=W271.W271D1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W272J021                                         
