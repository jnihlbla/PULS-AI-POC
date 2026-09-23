//W224J050 JOB (640W2240100W224J050,W100),'RTN W224D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W224    EXEC W224P050                                                         
//*                                                                             
//W22450.W22450D1 DD *                                                          
7179                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W224J050                                         
