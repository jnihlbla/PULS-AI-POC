//W221J36A JOB (670W2210100W221J36A,W100),'RTN W221D8',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W221    EXEC W221P36A                                                         
//*                                                                             
//W2216A.W2216AD1 DD *                                                          
4149                                                                            
//*                                                                             
//*                                                                             
//W2216AT EXEC WEMPTST,DSIN=W221.W221D8.W2216A(+1)                              
//PASSIV  EXEC WSOP,COND=(0,EQ,W2216AT.T)                                       
PASSIVATE W221J36C                                                              
PASSIVATE W221J36D                                                              
PASSIVATE W221Z5SE                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J36A                                         
