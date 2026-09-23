//W522J5SE JOB (650W5220100W522J5SE,W100),'RTN W522M1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V,TIME=2                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W522.W522M1.W52216A(+0)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//WZ14    EXEC WZ14DAP2,DSIN=W522.W522M1.W52216A(+0),CPU=5                      
//SYSIN           DD *                                                          
W52216-001                                                                      
W522SE                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W522.W522M1.W52216B(+0)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//WZ14    EXEC WZ14DAP2,DSIN=W522.W522M1.W52216B(+0),CPU=5                      
//SYSIN           DD *                                                          
W52216-002                                                                      
W522SE                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W522.W522M1.W52216D(+0)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//WZ14    EXEC WZ14DAP2,DSIN=W522.W522M1.W52216D(+0),CPU=5                      
//SYSIN           DD *                                                          
W52216-003                                                                      
W522SE                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W522J5SE                                         
