//W463J036 JOB (670W4630100W463J036,W100,1,5,0,1800),'RTN W463S5',              
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//        EXEC W463P036                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463J036                                         
