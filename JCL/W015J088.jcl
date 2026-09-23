//W015J088 JOB (640W0150100W015J088,W100),'RTN W015XX',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W015    EXEC W015P088                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W015J088                                         
