//W114J051 JOB (670W1140100W114J051,W100),'RTN W114S2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W114     EXEC W114P051                                                        
//*                                                                             
//SOP  EXEC WSOPEND,PROCESS=W114J051                                            
