//W221J06A JOB (670W2210200W221J06A,W100),'RTN W221D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W221    EXEC W221P06A                                                         
//*                                                                             
//W2216A.W2216AD1 DD *                                                          
7179                                                                            
//*                                                                             
//W2216AT EXEC WEMPTST,DSIN=W221.W221D3.W2216A(+1)                              
//PASSIV  EXEC WSOP,COND=(0,EQ,W2216AT.T)                                       
PASSIVATE W221J06C                                                              
PASSIVATE W221J06D                                                              
PASSIVATE W221Z2SE                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J06A                                         
