//W980JLME JOB (650W0090100W980JLME,W100),'RTN W980V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST9                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ    LOCAL                                                            
/*ROUTE PRINT  LOCAL                                                            
//*                                                                             
//W98028  EXEC W980P028                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W980JLME                                         
