//W221J059 JOB (670W2210100W221J059,W100),'RTN W200V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W221    EXEC W221P159,                                                        
//             INDIN2=W221.W200V1.W22159(+0)                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J059                                         
