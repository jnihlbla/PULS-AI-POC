//W114D1RE   JOB (650W1140100W114D1RE,W100),'RTN W114D1',                       
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM LINECT=0,FORMS=1800                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//SOP      EXEC WSOPEND,PROCESS=W114D1RE                                        
/*                                                                              
