//W330J008 JOB (670W3300100W330J008,W100),'RTN W330D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V,TIME=(5,0)                                               
/*JOBPARM LINES=99,CARDS=0,FORMS=1800                                           
//*+JBS BIND IMG0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W330P008 EXEC W330P008                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J008                                            
