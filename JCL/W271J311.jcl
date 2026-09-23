//W271J311 JOB (670W2710100W271J311,W100),'RTN W271DB',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W271    EXEC W271P011,                                                        
//             INDIN=W271.W271DB                                                
//*                                                                             
//SORT.SORTIN   DD  DSN=W271.W271DB.W27110(+0)                                  
//              DD  DUMMY                                                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J311                                         
