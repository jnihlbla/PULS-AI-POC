//W272J041 JOB (670W2720100W272J041,W100),'RTN W271D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W272    EXEC W272P041,                                                        
//             INDIN=W272.W271D1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W272J041                                         
