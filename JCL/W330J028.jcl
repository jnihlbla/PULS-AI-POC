//W330J028   JOB (670W3300100W330J028,W100),'RTN W330D1',                       
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
/*JOBPARM LINES=9,CARDS=0,FORMS=1800                                            
//*+JBS BIND IMG0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
//W330P028 EXEC W330P028                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J028                                            
