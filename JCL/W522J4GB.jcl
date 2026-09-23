//W522J4GB JOB (650W5220100W522J4GB,W100),'RTN W522M1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W522.W522M1.W52225(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W522.W522M1.W52225(+0)                                    
//SYSIN           DD *                                                          
W522GB-001                                                                      
W522GB                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W522J4GB                                         
