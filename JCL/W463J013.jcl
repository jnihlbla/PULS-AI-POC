//W463J013 JOB (670W4630100W463J013,W100,1,5,0,1800),'RTN W463S7',              
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ   LOCAL                                                             
//*+JBS BIND IMG0                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W463    EXEC W463P013                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463J013                                         
