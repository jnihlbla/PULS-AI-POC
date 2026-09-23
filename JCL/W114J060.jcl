//W114J060 JOB (650W1140100W114J060,W100),'RTN W114S2',                         
//             USER=?,PASSWORD=?,                                               
//          CLASS=K                                                             
/*JOBPARM LINES=99,FORMS=1800,LINECT=0                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//TOMTEST  EXEC WEMPTST,DSIN=WUT.W114S2.W11450(+0)                              
//ACTIVATE EXEC WSOP,                                                           
//             COND=(0,LT,TOMTEST.T)                                            
ACTIVATE W11450FI                                                               
//*                                                                             
//SOP      EXEC WSOPEND,PROCESS=W114J060                                        
/*                                                                              
