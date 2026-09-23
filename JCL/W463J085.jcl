//W463J085 JOB (670W4630100W463J085,W100),'RTN W463S8',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ   LOCAL                                                             
//*+JBS BIND IMG0                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W463    EXEC W463P085                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463J085                                         
