//W114J054 JOB (670W1140100W114J054,W100,1,5,0,1800),'RTN W114D6',              
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
//*+JBS BIND IMG0                                                               
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W114     EXEC W114P054                                                        
//*                                                                             
//SOP  EXEC WSOPEND,PROCESS=W114J054                                            
