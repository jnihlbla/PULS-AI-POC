//W122J050 JOB (650W1220100W122J050,W100),'RTN W122B1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W122    EXEC W122P050                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W122J050                                         
