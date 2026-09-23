//W224J263 JOB (640W2240100W224J263,W100),'RTN W224V4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W224    EXEC W224P263                                                         
//W22463.W22463D1 DD *                                                          
VEC4149                                                                         
//*                                                                             
//W22463T EXEC WEMPTST,DSIN=W224.W224V4.W22463(+1)                              
//PASSIV  EXEC WSOP,COND=(0,EQ,W22463T.T)                                       
PASSIVATE W221J46C                                                              
PASSIVATE W221J46D                                                              
PASSIVATE W221ZDSE                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W224J263                                         
