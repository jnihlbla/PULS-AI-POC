//W114J050 JOB (670W1140100W114J050,W100,1,5,0,1800),'RTN W114S2',              
//             USER=?,PASSWORD=?,                                               
//             CLASS=1                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W114     EXEC W114P050                                                        
//*                                                                             
//SOP  EXEC WSOPEND,PROCESS=W114J050                                            
