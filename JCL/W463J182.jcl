//W463J182 JOB (670W4630100W463J182,W100),'RTN W463S6',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W463    EXEC W463P082,INDIN=W463.W463S6                                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463J182                                         
