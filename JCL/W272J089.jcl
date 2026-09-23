//W272J089 JOB (670W2720100W272J089,W100),'RTN W271V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W272    EXEC W272P089,                                                        
//             INDIN=W272.W271V1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W272J089                                         
