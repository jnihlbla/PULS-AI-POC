//W224J063 JOB (640W2240100W224J063,W100),'RTN W224V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W224    EXEC W224P063                                                         
//W22463.W22463D1 DD *                                                          
VEC7179                                                                         
//*                                                                             
//W22463T EXEC WEMPTST,DSIN=W224.W224V2.W22463(+1)                              
//PASSIV  EXEC WSOP,COND=(0,EQ,W22463T.T)                                       
PASSIVATE W221J16C                                                              
PASSIVATE W221J16D                                                              
PASSIVATE W221ZBSE                                                              
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W224J063                                         
