//W463J064 JOB (670W4630100W463J064,W100,1,5,0,1800),'RTN W463V1',              
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//        EXEC W463P064                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463J064                                         
