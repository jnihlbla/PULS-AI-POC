//W463J057 JOB (670W4630100W463J057,W100),'RTN W463S9',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//W463    EXEC W463P057                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463J057                                         
