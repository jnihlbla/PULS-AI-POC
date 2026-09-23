//W330J030 JOB (670W3300100W330J030,W100),'RTN W330D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
/*JOBPARM LINES=99,CARDS=0,FORMS=1800                                           
//*+JBS BIND IMG0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W330P030 EXEC W330P030                                                        
//*                                                                             
//W33031T EXEC WEMPTST,DSIN=W330.W330D1.W33031(+1)                              
//*                    TESTA PÅ (+1) DÅ CATLG SKER EFTER JOBBET                 
//AKTIVB1 EXEC WSOP,COND=(0,LT,W33031T.T)                                       
ACTIVATE W330B1                                                                 
//*                                                                             
//W33035T EXEC WEMPTST,DSIN=W330.W330D1.W33035(+1)                              
//AKTIVB2 EXEC WSOP,COND=(0,LT,W33035T.T)                                       
ACTIVATE W330B2                                                                 
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J030                                            
